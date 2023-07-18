// { dg-additional-options "-w" }
#[lang = "sized"]
pub trait Sized {}

struct FatPtr<T> {
    data: *const T,
    len: usize,
}

pub union Repr<T> {
    rust: *const [T],
    rust_mut: *mut [T],
    raw: FatPtr<T>,
}

impl<T> [T] {
    pub const fn is_empty(&self) -> bool {
        self.len() == 0
    }

    pub const fn len(&self) -> usize {
        unsafe { Repr { rust: self }.raw.len }
    }
}
