// { dg-skip-if "" { *-*-* } { "-m32" } { "" } }
// { dg-options "-w" }
// { dg-output "Hash: 0x63d53fd2170bbb8c\r*\n" }
#![feature(intrinsics)]
#![feature(rustc_attrs)]

#[lang = "sized"]
trait Sized {}

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn wrapping_add<T>(a: T, b: T) -> T;
        pub fn rotate_left<T>(a: T, b: T) -> T;
        pub fn offset<T>(ptr: *const T, count: isize) -> *const T;
    }
}

#[lang = "add"]
trait Add<Rhs = Self> {
    type Output;

    fn add(self, rhs: Rhs) -> Self::Output;
}

macro_rules! add_impl {
    ($($t:ty)*) => ($(
        impl Add for $t {
            type Output = $t;

            #[inline]
            #[rustc_inherit_overflow_checks]
            fn add(self, other: $t) -> $t { self + other }
        }


    )*)
}

add_impl! { usize u8 u16 u32 u64 u128 isize i8 i16 i32 i64 i128 f32 f64 }

impl<T> *const T {
    pub unsafe fn add(self, count: usize) -> Self {
        // SAFETY: the caller must uphold the safety contract for `offset`.
        unsafe { self.offset(count as isize) }
    }

    pub unsafe fn offset(self, count: isize) -> *const T {
        // SAFETY: the caller must uphold the safety contract for `offset`.
        unsafe { intrinsics::offset(self, count) }
    }
}

macro_rules! impl_uint {
    ($($ty:ident = $lang:literal),*) => {
        $(
            #[lang = $lang]
            impl $ty {
                pub fn wrapping_add(self, rhs: Self) -> Self {
                    intrinsics::wrapping_add(self, rhs)
                }

                pub fn rotate_left(self, n: u32) -> Self {
                    intrinsics::rotate_left(self, n as Self)
                }

                pub fn to_le(self) -> Self {
                    #[cfg(target_endian = "little")]
                    {
                        self
                    }
                    #[cfg(not(target_endian = "little"))]
                    {
                       self.swap_bytes()
                    }
                }
            }
        )*
    }
}

impl_uint!(
    u8 = "u8",
    u16 = "u16",
    u32 = "u32",
    u64 = "u64",
    u128 = "u128",
    usize = "usize"
);

#[repr(C)]
pub(crate) struct SliceComponents {
    pub(crate) data_address: *const (),
    pub(crate) metadata: usize,
}

#[repr(C)]
pub(crate) union SliceRepr<T> {
    pub(crate) const_ptr: *const [T],
    pub(crate) mut_ptr: *mut [T],
    pub(crate) components: SliceComponents,
}

impl<T> [T] {
    pub const fn as_ptr(&self) -> *const T {
        self as *const [T] as *const T
    }

    pub unsafe fn get_unchecked(&self, index: usize) -> &T {
        unsafe { &*self.as_ptr().add(index) }
    }

    pub fn len(&self) -> usize {
        unsafe {
            SliceRepr {
                const_ptr: self as *const _,
            }
            .components
            .metadata
        }
    }
}

trait HasherTrait {
    fn write(&mut self, msg: &[u8]);
    fn finish(&self) -> u64;
}

mod cmp {
    pub fn min(a: usize, b: usize) -> usize {
        if a < b {
            a
        } else {
            b
        }
    }
}

struct PhantomData<T>;

mod mem {
    extern "rust-intrinsic" {
        fn transmute<T, U>(_: T) -> U;
        fn size_of<T>() -> usize;
    }
}

mod ptr {
    extern "rust-intrinsic" {
        fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
    }
}

#[repr(C)]
struct State {
    v0: u64,
    v2: u64,
    v1: u64,
    v3: u64,
}

struct Hasher<S: Sip> {
    k0: u64,
    k1: u64,
    length: usize, // how many bytes we've processed
    state: State,  // hash State
    tail: u64,     // unprocessed bytes le
    ntail: usize,  // how many bytes in tail are valid
    _marker: PhantomData<S>,
}

macro_rules! compress {
    ($state:expr) => {{
        compress!($state.v0, $state.v1, $state.v2, $state.v3)
    }};
    ($v0:expr, $v1:expr, $v2:expr, $v3:expr) => {{
        $v0 = $v0.wrapping_add($v1);
        $v1 = $v1.rotate_left(13);
        $v1 ^= $v0;
        $v0 = $v0.rotate_left(32);
        $v2 = $v2.wrapping_add($v3);
        $v3 = $v3.rotate_left(16);
        $v3 ^= $v2;
        $v0 = $v0.wrapping_add($v3);
        $v3 = $v3.rotate_left(21);
        $v3 ^= $v0;
        $v2 = $v2.wrapping_add($v1);
        $v1 = $v1.rotate_left(17);
        $v1 ^= $v2;
        $v2 = $v2.rotate_left(32);
    }};
}

#[doc(hidden)]
trait Sip {
    fn c_rounds(_: &mut State);
    fn d_rounds(_: &mut State);
}

struct Sip13Rounds;

impl Sip for Sip13Rounds {
    #[inline]
    fn c_rounds(state: &mut State) {
        compress!(state);
    }

    #[inline]
    fn d_rounds(state: &mut State) {
        compress!(state);
        compress!(state);
        compress!(state);
    }
}

struct Sip24Rounds;

impl Sip for Sip24Rounds {
    #[inline]
    fn c_rounds(state: &mut State) {
        compress!(state);
        compress!(state);
    }

    #[inline]
    fn d_rounds(state: &mut State) {
        compress!(state);
        compress!(state);
        compress!(state);
        compress!(state);
    }
}

pub struct SipHasher13 {
    hasher: Hasher<Sip13Rounds>,
}

struct SipHasher24 {
    hasher: Hasher<Sip24Rounds>,
}

pub struct SipHasher(SipHasher24);

macro_rules! load_int_le {
    ($buf:expr, $i:expr, $int_ty:ident) => {{
        let mut data = 0 as $int_ty;
        ptr::copy_nonoverlapping(
            $buf.as_ptr().add($i),
            &mut data as *mut _ as *mut u8,
            mem::size_of::<$int_ty>(),
        );
        data.to_le()
    }};
}

#[inline]
unsafe fn u8to64_le(buf: &[u8], start: usize, len: usize) -> u64 {
    let mut i = 0; // current byte index (from LSB) in the output u64
    let mut out = 0;
    if i + 3 < len {
        // SAFETY: `i` cannot be greater than `len`, and the caller must guarantee
        // that the index start..start+len is in bounds.
        out = unsafe { load_int_le!(buf, start + i, u32) } as u64;
        i += 4;
    }
    if i + 1 < len {
        // SAFETY: same as above.
        out |= (unsafe { load_int_le!(buf, start + i, u16) } as u64) << ((i * 8) as u64);
        i += 2
    }
    if i < len {
        // SAFETY: same as above.
        out |= (unsafe { *buf.get_unchecked(start + i) } as u64) << ((i * 8) as u64);
        i += 1;
    }
    out
}

impl SipHasher {
    #[inline]
    #[must_use]
    pub fn new() -> SipHasher {
        SipHasher::new_with_keys(0, 0)
    }

    #[inline]
    #[must_use]
    pub fn new_with_keys(key0: u64, key1: u64) -> SipHasher {
        SipHasher(SipHasher24 {
            hasher: Hasher::new_with_keys(key0, key1),
        })
    }
}

impl SipHasher13 {
    #[inline]
    pub fn new() -> SipHasher13 {
        SipHasher13::new_with_keys(0, 0)
    }

    #[inline]
    pub fn new_with_keys(key0: u64, key1: u64) -> SipHasher13 {
        SipHasher13 {
            hasher: Hasher::new_with_keys(key0, key1),
        }
    }
}

impl<S: Sip> Hasher<S> {
    #[inline]
    fn new_with_keys(key0: u64, key1: u64) -> Hasher<S> {
        let mut state = Hasher {
            k0: key0,
            k1: key1,
            length: 0,
            state: State {
                v0: 0,
                v1: 0,
                v2: 0,
                v3: 0,
            },
            tail: 0,
            ntail: 0,
            _marker: PhantomData,
        };
        state.reset();
        state
    }

    #[inline]
    fn reset(&mut self) {
        self.length = 0;
        self.state.v0 = self.k0 ^ 0x736f6d6570736575;
        self.state.v1 = self.k1 ^ 0x646f72616e646f6d;
        self.state.v2 = self.k0 ^ 0x6c7967656e657261;
        self.state.v3 = self.k1 ^ 0x7465646279746573;
        self.ntail = 0;
    }
}

impl HasherTrait for SipHasher {
    #[inline]
    fn write(&mut self, msg: &[u8]) {
        self.0.hasher.write(msg)
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.0.hasher.finish()
    }
}

impl HasherTrait for SipHasher13 {
    #[inline]
    fn write(&mut self, msg: &[u8]) {
        self.hasher.write(msg)
    }

    #[inline]
    fn finish(&self) -> u64 {
        self.hasher.finish()
    }
}

impl<S: Sip> HasherTrait for Hasher<S> {
    #[inline]
    fn write(&mut self, msg: &[u8]) {
        let length = msg.len();
        self.length += length;

        let mut needed = 0;

        if self.ntail != 0 {
            needed = 8 - self.ntail;
            // SAFETY: `cmp::min(length, needed)` is guaranteed to not be over `length`
            self.tail |=
                unsafe { u8to64_le(msg, 0, cmp::min(length, needed)) } << ((8 * self.ntail) as u64);
            if length < needed {
                self.ntail += length;
                return;
            } else {
                self.state.v3 ^= self.tail;
                S::c_rounds(&mut self.state);
                self.state.v0 ^= self.tail;
                self.ntail = 0;
            }
        }

        // Buffered tail is now flushed, process new input.
        let len = length - needed;
        let left = len & 0x7; // len % 8

        let mut i = needed;
        while i < len - left {
            let mi = unsafe { load_int_le!(msg, i, u64) };

            self.state.v3 ^= mi;
            S::c_rounds(&mut self.state);
            self.state.v0 ^= mi;

            i += 8;
        }

        self.tail = unsafe { u8to64_le(msg, i, left) };
        self.ntail = left;
    }

    #[inline]
    fn finish(&self) -> u64 {
        let mut state = self.state;

        let b: u64 = ((self.length as u64 & 0xff) << 56) | self.tail;

        state.v3 ^= b;
        S::c_rounds(&mut state);
        state.v0 ^= b;

        state.v2 ^= 0xff;
        S::d_rounds(&mut state);

        state.v0 ^ state.v1 ^ state.v2 ^ state.v3
    }
}

extern "C" {
    fn printf(fmt: *const u8, ...) -> i32;
}

fn main() -> i32 {
    let mut hasher = SipHasher::new_with_keys(0x0706050403020100, 0x0f0e0d0c0b0a0908);
    hasher.write(b"Hello");
    let result = hasher.finish();

    unsafe {
        printf("Hash: 0x%016llx\n\0" as *const str as *const u8, result);
    }

    0
}
