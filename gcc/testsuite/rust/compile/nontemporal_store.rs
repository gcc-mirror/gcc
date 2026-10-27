#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

extern "rust-intrinsic" {
    fn nontemporal_store<T>(ptr: *mut T, value: T);
}

fn main() {
    let mut a = 15i32;

    unsafe {
        nontemporal_store(&mut a as *mut i32, 16);
    }
}
