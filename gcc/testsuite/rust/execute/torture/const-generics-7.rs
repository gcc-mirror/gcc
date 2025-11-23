#![feature(intrinsics)]

#![feature(lang_items)]
#[lang = "sized"]
pub trait Sized {}

mod mem {
    extern "rust-intrinsic" {
        #[rustc_const_stable(feature = "const_size_of", since = "1.40.0")]
        pub fn size_of<T>() -> usize;
    }
}

struct Foo<T>;

impl<T> Foo<T> {
    const MAGIC: usize = mem::size_of::<T>();
}

fn main() -> i32 {
    let sz = Foo::<u16>::MAGIC;
    sz as i32 - 2
}
