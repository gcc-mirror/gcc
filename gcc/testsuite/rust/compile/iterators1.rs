#![feature(intrinsics)]

pub use option::Option::{self, None, Some};
pub use result::Result::{self, Err, Ok};

extern "C" {
    fn printf(s: *const i8, ...);
}

mod option {
    pub enum Option<T> {
        None,
        Some(T),
    }
}

mod result {
    enum Result<T, E> {
        Ok(T),
        Err(E),
    }
}

#[lang = "sized"]
pub trait Sized {}

#[lang = "clone"]
pub trait Clone: Sized {
    fn clone(&self) -> Self;

    fn clone_from(&mut self, source: &Self) {
        *self = source.clone()
    }
}

mod impls {
    use super::Clone;

    macro_rules! impl_clone {
        ($($t:ty)*) => {
            $(
                impl Clone for $t {
                    fn clone(&self) -> Self {
                        *self
                    }
                }
            )*
        }
    }

    impl_clone! {
        usize u8 u16 u32 u64 // u128
        isize i8 i16 i32 i64 // i128
        f32 f64
        bool char
    }
}

#[lang = "copy"]
pub trait Copy: Clone {
    // Empty.
}

mod copy_impls {
    use super::Copy;

    macro_rules! impl_copy {
        ($($t:ty)*) => {
            $(
                impl Copy for $t {}
            )*
        }
    }

    impl_copy! {
        usize u8 u16 u32 u64 // u128
        isize i8 i16 i32 i64 // i128
        f32 f64
        bool char
    }
}

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn add_with_overflow<T>(x: T, y: T) -> (T, bool);
        pub fn wrapping_add<T>(a: T, b: T) -> T;
        pub fn wrapping_sub<T>(a: T, b: T) -> T;
        pub fn rotate_left<T>(a: T, b: T) -> T;
        pub fn rotate_right<T>(a: T, b: T) -> T;
        pub fn offset<T>(ptr: *const T, count: isize) -> *const T;
        pub fn copy_nonoverlapping<T>(src: *const T, dst: *mut T, count: usize);
        pub fn move_val_init<T>(dst: *mut T, src: T);
        pub fn uninit<T>() -> T;
    }
}

mod ptr {
    #[lang = "const_ptr"]
    impl<T> *const T {
        pub unsafe fn offset(self, count: isize) -> *const T {
            intrinsics::offset(self, count)
        }
    }

    #[lang = "mut_ptr"]
    impl<T> *mut T {
        pub unsafe fn offset(self, count: isize) -> *mut T {
            intrinsics::offset(self, count) as *mut T
        }
    }

    pub unsafe fn swap_nonoverlapping<T>(x: *mut T, y: *mut T, count: usize) {
        let x = x as *mut u8;
        let y = y as *mut u8;
        let len = mem::size_of::<T>() * count;
        swap_nonoverlapping_bytes(x, y, len)
    }

    pub unsafe fn swap_nonoverlapping_one<T>(x: *mut T, y: *mut T) {
        // For types smaller than the block optimization below,
        // just swap directly to avoid pessimizing codegen.
        if mem::size_of::<T>() < 32 {
            let z = read(x);
            intrinsics::copy_nonoverlapping(y, x, 1);
            write(y, z);
        } else {
            swap_nonoverlapping(x, y, 1);
        }
    }

    pub unsafe fn write<T>(dst: *mut T, src: T) {
        intrinsics::move_val_init(&mut *dst, src)
    }

    pub unsafe fn read<T>(src: *const T) -> T {
        let mut tmp: T = mem::uninitialized();
        intrinsics::copy_nonoverlapping(src, &mut tmp, 1);
        tmp
    }

    unsafe fn swap_nonoverlapping_bytes(x: *mut u8, y: *mut u8, len: usize) {
        struct Block(u64, u64, u64, u64);
        struct UnalignedBlock(u64, u64, u64, u64);

        let block_size = mem::size_of::<Block>();

        // Loop through x & y, copying them `Block` at a time
        // The optimizer should unroll the loop fully for most types
        // N.B. We can't use a for loop as the `range` impl calls `mem::swap` recursively
        let mut i: usize = 0;
        while i + block_size <= len {
            // Create some uninitialized memory as scratch space
            // Declaring `t` here avoids aligning the stack when this loop is unused
            let mut t: Block = mem::uninitialized();
            let t = &mut t as *mut _ as *mut u8;
            let x = x.offset(i as isize);
            let y = y.offset(i as isize);

            // Swap a block of bytes of x & y, using t as a temporary buffer
            // This should be optimized into efficient SIMD operations where available
            intrinsics::copy_nonoverlapping(x, t, block_size);
            intrinsics::copy_nonoverlapping(y, x, block_size);
            intrinsics::copy_nonoverlapping(t, y, block_size);
            i += block_size;
        }

        if i < len {
            // Swap any remaining bytes
            let mut t: UnalignedBlock = mem::uninitialized();
            let rem = len - i;

            let t = &mut t as *mut _ as *mut u8;
            let x = x.offset(i as isize);
            let y = y.offset(i as isize);

            intrinsics::copy_nonoverlapping(x, t, rem);
            intrinsics::copy_nonoverlapping(y, x, rem);
            intrinsics::copy_nonoverlapping(t, y, rem);
        }
    }
}

mod mem {
    extern "rust-intrinsic" {
        #[rustc_const_stable(feature = "const_transmute", since = "1.46.0")]
        pub fn transmute<T, U>(_: T) -> U;
        #[rustc_const_stable(feature = "const_size_of", since = "1.40.0")]
        pub fn size_of<T>() -> usize;
    }

    pub fn swap<T>(x: &mut T, y: &mut T) {
        unsafe {
            ptr::swap_nonoverlapping_one(x, y);
        }
    }

    pub fn replace<T>(dest: &mut T, mut src: T) -> T {
        swap(dest, &mut src);
        src
    }

    pub unsafe fn uninitialized<T>() -> T {
        intrinsics::uninit()
    }
}

macro_rules! impl_uint {
    ($($ty:ident = $lang:literal),*) => {
        $(
            impl $ty {
                pub fn wrapping_add(self, rhs: Self) -> Self {
                    unsafe {
                        intrinsics::wrapping_add(self, rhs)
                    }
                }

                pub fn wrapping_sub(self, rhs: Self) -> Self {
                    unsafe {
                        intrinsics::wrapping_sub(self, rhs)
                    }
                }

                pub fn rotate_left(self, n: u32) -> Self {
                    unsafe {
                        intrinsics::rotate_left(self, n as Self)
                    }
                }

                pub fn rotate_right(self, n: u32) -> Self {
                    unsafe {
                        intrinsics::rotate_right(self, n as Self)
                    }
                }

                pub fn to_le(self) -> Self {
                    #[cfg(target_endian = "little")]
                    {
                        self
                    }
                }

                pub const fn from_le_bytes(bytes: [u8; mem::size_of::<Self>()]) -> Self {
                    Self::from_le(Self::from_ne_bytes(bytes))
                }

                pub const fn from_le(x: Self) -> Self {
                    #[cfg(target_endian = "little")]
                    {
                        x
                    }
                }

                pub const fn from_ne_bytes(bytes: [u8; mem::size_of::<Self>()]) -> Self {
                    unsafe { mem::transmute(bytes) }
                }

                pub fn checked_add(self, rhs: Self) -> Option<Self> {
                    let (a, b) = self.overflowing_add(rhs);
                    if b {
                        Option::None
                    } else {
                        Option::Some(a)
                    }
                }

                pub fn overflowing_add(self, rhs: Self) -> (Self, bool) {
                    let (a, b) = unsafe { intrinsics::add_with_overflow(self as $ty, rhs as $ty) };
                    (a as Self, b)
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
    usize = "usize"
);

#[lang = "add"]
pub trait Add<RHS = Self> {
    type Output;

    fn add(self, rhs: RHS) -> Self::Output;
}
macro_rules! add_impl {
    ($($t:ty)*) => ($(
        impl Add for $t {
            type Output = $t;

            fn add(self, other: $t) -> $t { self + other }
        }
    )*)
}

add_impl! { usize u8 u16 u32 u64  /*isize i8 i16 i32 i64*/  f32 f64 }

#[lang = "sub"]
pub trait Sub<RHS = Self> {
    type Output;

    fn sub(self, rhs: RHS) -> Self::Output;
}
macro_rules! sub_impl {
    ($($t:ty)*) => ($(
        impl Sub for $t {
            type Output = $t;

            fn sub(self, other: $t) -> $t { self - other }
        }
    )*)
}

sub_impl! { usize u8 u16 u32 u64  /*isize i8 i16 i32 i64*/  f32 f64 }

#[lang = "Range"]
pub struct Range<Idx> {
    pub start: Idx,
    pub end: Idx,
}

pub trait TryFrom<T>: Sized {
    /// The type returned in the event of a conversion error.
    type Error;

    /// Performs the conversion.
    fn try_from(value: T) -> Result<Self, Self::Error>;
}

pub trait From<T>: Sized {
    fn from(_: T) -> Self;
}

impl<T> From<T> for T {
    fn from(t: T) -> T {
        t
    }
}

impl<T, U> TryFrom<U> for T
where
    T: From<U>,
{
    type Error = !;

    fn try_from(value: U) -> Result<Self, Self::Error> {
        Ok(T::from(value))
    }
}

trait Step {
    /// Returns the number of steps between two step objects. The count is
    /// inclusive of `start` and exclusive of `end`.
    ///
    /// Returns `None` if it is not possible to calculate `steps_between`
    /// without overflow.
    fn steps_between(start: &Self, end: &Self) -> Option<usize>;

    /// Replaces this step with `1`, returning itself
    fn replace_one(&mut self) -> Self;

    /// Replaces this step with `0`, returning itself
    fn replace_zero(&mut self) -> Self;

    /// Adds one to this step, returning the result
    fn add_one(&self) -> Self;

    /// Subtracts one to this step, returning the result
    fn sub_one(&self) -> Self;

    /// Add an usize, returning None on overflow
    fn add_usize(&self, n: usize) -> Option<Self>;
}

// These are still macro-generated because the integer literals resolve to different types.
macro_rules! step_identical_methods {
    () => {
        #[inline]
        fn replace_one(&mut self) -> Self {
            mem::replace(self, 1)
        }

        #[inline]
        fn replace_zero(&mut self) -> Self {
            mem::replace(self, 0)
        }

        #[inline]
        fn add_one(&self) -> Self {
            Add::add(*self, 1)
        }

        #[inline]
        fn sub_one(&self) -> Self {
            Sub::sub(*self, 1)
        }
    };
}

macro_rules! step_impl_unsigned {
    ($($t:ty)*) => ($(
        impl Step for $t {
            fn steps_between(start: &$t, end: &$t) -> Option<usize> {
                if *start < *end {
                    // Note: We assume $t <= usize here
                    Option::Some((*end - *start) as usize)
                } else {
                    Option::Some(0)
                }
            }

            fn add_usize(&self, n: usize) -> Option<Self> {
                match <$t>::try_from(n) {
                    Result::Ok(n_as_t) => self.checked_add(n_as_t),
                    Result::Err(_) => Option::None,
                }
            }

            step_identical_methods!();
        }
    )*)
}
macro_rules! step_impl_signed {
    ($( [$t:ty : $unsigned:ty] )*) => ($(
        impl Step for $t {
            #[inline]
            #[allow(trivial_numeric_casts)]
            fn steps_between(start: &$t, end: &$t) -> Option<usize> {
                if *start < *end {
                    // Note: We assume $t <= isize here
                    // Use .wrapping_sub and cast to usize to compute the
                    // difference that may not fit inside the range of isize.
                    Option::Some((*end as isize).wrapping_sub(*start as isize) as usize)
                } else {
                    Option::Some(0)
                }
            }

            #[inline]
            #[allow(unreachable_patterns)]
            fn add_usize(&self, n: usize) -> Option<Self> {
                match <$unsigned>::try_from(n) {
                    Result::Ok(n_as_unsigned) => {
                        // Wrapping in unsigned space handles cases like
                        // `-120_i8.add_usize(200) == Option::Some(80_i8)`,
                        // even though 200_usize is out of range for i8.
                        let wrapped = (*self as $unsigned).wrapping_add(n_as_unsigned) as $t;
                        if wrapped >= *self {
                            Option::Some(wrapped)
                        } else {
                            Option::None  // Addition overflowed
                        }
                    }
                    Result::Err(_) => Option::None,
                }
            }

            step_identical_methods!();
        }
    )*)
}

macro_rules! step_impl_no_between {
    ($($t:ty)*) => ($(
        impl Step for $t {
            #[inline]
            fn steps_between(_start: &Self, _end: &Self) -> Option<usize> {
                Option::None
            }

            #[inline]
            fn add_usize(&self, n: usize) -> Option<Self> {
                self.checked_add(n as $t)
            }

            step_identical_methods!();
        }
    )*)
}

step_impl_unsigned!(usize);

pub trait Iterator {
    type Item;

    fn next(&mut self) -> Option<Self::Item>;
}

impl<A: Step> Iterator for Range<A> {
    type Item = A;

    fn next(&mut self) -> Option<A> {
        if self.start < self.end {
            // We check for overflow here, even though it can't actually
            // happen. Adding this check does however help llvm vectorize loops
            // for some ranges that don't get vectorized otherwise,
            // and this won't actually result in an extra check in an optimized build.
            match self.start.add_usize(1) {
                Option::Some(mut n) => {
                    mem::swap(&mut n, &mut self.start);
                    Option::Some(n)
                }
                Option::None => Option::None,
            }
        } else {
            Option::None
        }
    }
}

pub trait IntoIterator {
    type Item;

    type IntoIter: Iterator<Item = Self::Item>;

    fn into_iter(self) -> Self::IntoIter;
}

impl<I: Iterator> IntoIterator for I {
    type Item = I::Item;
    type IntoIter = I;

    fn into_iter(self) -> I {
        self
    }
}

pub fn main() -> i32 {
    let a = 1..3;
    let it = a.into_iter();
    loop {
        let n = it.next();
        match (n) {
            Option::Some(v) => unsafe {
                let a = "Hello World %d\n\0";
                let b = a as *const str;
                let c = b as *const i8;

                printf(c, v);
            },
            Option::None => {
                break;
            }
        }
    }

    0
}
