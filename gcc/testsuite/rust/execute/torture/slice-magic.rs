// { dg-additional-options "-w" }
#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    #[rustc_const_stable(feature = "const_ptr_offset", since = "1.61.0")]
    pub fn offset<T>(dst: *const T, offset: isize) -> *const T;
}

struct FatPtr<T> {
    data: *const T,
    len: usize,
}

union Repr<T> {
    rust: *const [T],
    rust_mut: *mut [T],
    raw: FatPtr<T>,
}

#[lang = "Range"]
pub struct Range<Idx> {
    pub start: Idx,
    pub end: Idx,
}

#[lang = "const_slice_ptr"]
impl<A> *const [A] {
    pub const fn len(self) -> usize {
        unsafe { Repr { rust: self }.raw.len }
    }

    pub const fn as_ptr(self) -> *const A {
        self as *const A
    }
}

#[lang = "const_ptr"]
impl<B> *const B {
    pub const unsafe fn offset(self, count: isize) -> *const B {
        unsafe { offset(self, count) }
    }

    pub const unsafe fn add(self, count: usize) -> Self {
        unsafe { self.offset(count as isize) }
    }

    pub const fn as_ptr(self) -> *const B {
        self as *const B
    }
}

const fn slice_from_raw_parts<C>(data: *const C, len: usize) -> *const [C] {
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

pub unsafe trait SliceIndex<X> {
    type Output;

    unsafe fn get_unchecked(self, slice: *const X) -> *const Self::Output;

    fn index(self, slice: &X) -> &Self::Output;
}

unsafe impl<Y> SliceIndex<[Y]> for Range<usize> {
    type Output = [Y];

    unsafe fn get_unchecked(self, slice: *const [Y]) -> *const [Y] {
        unsafe {
            let a: *const Y = slice.as_ptr();
            let b: *const Y = a.add(self.start);
            slice_from_raw_parts(b, self.end - self.start)
        }
    }

    fn index(self, slice: &[Y]) -> &[Y] {
        unsafe { &*self.get_unchecked(slice) }
    }
}

impl<T, I> Index<I> for [T]
where
    I: SliceIndex<[T]>,
{
    type Output = I::Output;

    fn index(&self, index: I) -> &I::Output {
        index.index(self)
    }
}

fn main() -> i32 {
    let a = [1, 2, 3, 4, 5];
    let b = &a[1..3];

    0
}
