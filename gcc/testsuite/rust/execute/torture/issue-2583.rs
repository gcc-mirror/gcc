#[lang = "sized"]
pub trait Sized {}

mod intrinsics {
    extern "rust-intrinsic" {
        pub fn uninit<T>() -> T;
    }
}

pub fn main() -> i32 {
    let _val: usize = unsafe { intrinsics::uninit() };
    0
}
