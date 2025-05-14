// { dg-options "-w" }

#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

mod intrinsics {
    extern "rust-intrinsic" {
        #[rustc_const_stable(feature = "const_ptr_offset", since = "1.61.0")]
        pub fn offset<T>(ptr: *const T, count: isize) -> *const T;
    }
}

mod mem {
    extern "rust-intrinsic" {
        fn size_of<T>() -> usize;
    }
}

extern "C" {
    fn printf(s: *const i8, ...);
}

struct FatPtr<T> {
    data: *const T,
    len: usize,
}

pub union Repr<T> {
    rust: *const [T],
    rust_mut: *mut [T],
    raw: FatPtr<T>,
}

pub enum Option<T> {
    None,
    Some(T),
}

#[lang = "Range"]
pub struct Range<Idx> {
    pub start: Idx,
    pub end: Idx,
}

#[lang = "const_slice_ptr"]
impl<T> *const [T] {
    pub const fn len(self) -> usize {
        unsafe { Repr { rust: self }.raw.len }
    }

    pub const fn as_ptr(self) -> *const T {
        self as *const T
    }
}

#[lang = "const_ptr"]
impl<T> *const T {
    pub const unsafe fn offset(self, count: isize) -> *const T {
        unsafe { intrinsics::offset(self, count) }
    }

    pub const unsafe fn add(self, count: usize) -> Self {
        unsafe { self.offset(count as isize) }
    }

    pub const fn as_ptr(self) -> *const T {
        self as *const T
    }
}

const fn slice_from_raw_parts<T>(data: *const T, len: usize) -> *const [T] {
    unsafe {
        Repr {
            raw: FatPtr { data, len },
        }
        .rust
    }
}

#[lang = "index"]
trait Index<Idx> {
    type Output;

    fn index(&self, index: Idx) -> &Self::Output;
}

impl<T> [T] {
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn len(&self) -> usize {
        unsafe { Repr { rust: self }.raw.len }
    }
}

pub unsafe trait SliceIndex<T> {
    type Output;

    fn get(self, slice: &T) -> Option<&Self::Output>;

    unsafe fn get_unchecked(self, slice: *const T) -> *const Self::Output;

    fn index(self, slice: &T) -> &Self::Output;
}

unsafe impl<T> SliceIndex<[T]> for usize {
    type Output = T;

    fn get(self, slice: &[T]) -> Option<&T> {
        unsafe { Option::Some(&*self.get_unchecked(slice)) }
    }

    unsafe fn get_unchecked(self, slice: *const [T]) -> *const T {
        // SAFETY: the caller guarantees that `slice` is not dangling, so it
        // cannot be longer than `isize::MAX`. They also guarantee that
        // `self` is in bounds of `slice` so `self` cannot overflow an `isize`,
        // so the call to `add` is safe.
        unsafe { slice.as_ptr().add(self) }
    }

    fn index(self, slice: &[T]) -> &T {
        unsafe { &*self.get_unchecked(slice) }
    }
}

unsafe impl<T> SliceIndex<[T]> for Range<usize> {
    type Output = [T];

    fn get(self, slice: &[T]) -> Option<&[T]> {
        if self.start > self.end || self.end > slice.len() {
            Option::None
        } else {
            unsafe { Option::Some(&*self.get_unchecked(slice)) }
        }
    }

    unsafe fn get_unchecked(self, slice: *const [T]) -> *const [T] {
        unsafe {
            let a: *const T = slice.as_ptr();
            let b: *const T = a.add(self.start);
            slice_from_raw_parts(b, self.end - self.start)
        }
    }

    fn index(self, slice: &[T]) -> &[T] {
        unsafe { &*self.get_unchecked(slice) }
    }
}

impl<T, I> Index<I> for [T]
where
    I: SliceIndex<[T]>,
{
    type Output = I::Output;

    fn index(&self, index: I) -> &I::Output {
        unsafe {
            let a = "slice-index\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }

        index.index(self)
    }
}

fn main() -> i32 {
    let a = [1, 2, 3, 4, 5];
    let b = a[1];

    b - 2
}
