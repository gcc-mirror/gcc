#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

#[lang = "sized"]
trait Sized {}

extern "rust-intrinsic" {
    fn pref_align_of<T>();
}

fn foo() {
    unsafe {
        let _ = pref_align_of::<i32>(); // { dg-message "sorry, unimplemented" }
    };
}

fn main() {
    foo();
}
