#![feature(no_core, intrinsics, lang_items, staged_api)]
#![no_core]

#[lang = "sized"]
pub trait Sized {}

mod my_u32 {
    impl u32 {
        pub const fn to_be_bytes(self) -> [u8; crate::mem::size_of::<Self>()] {
            [0; crate::mem::size_of::<Self>()]
        }

        pub fn other_method(self) -> u32 {
            self
        }
    }
}

mod my_f32 {
    impl f32 {
        pub fn to_bits(self) -> u32 {
            0
        }

        pub fn do_something(self) -> u32 {
            self.to_bits().other_method()
        }
    }
}

extern "rust-intrinsic" {
    #[rustc_const_stable(feature = "const_size_of", since = "1.40.0")]
    pub fn size_of<T>() -> usize;
}

mod mem {
    pub const fn size_of<T>() -> usize {
        crate::size_of::<T>()
    }
}

fn main() -> i32 {
    let x: f32 = 0.0;
    let _ = x.do_something();
    0
}
