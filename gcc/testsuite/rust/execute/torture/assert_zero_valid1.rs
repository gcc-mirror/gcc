#![feature(no_core)]
#![feature(intrinsics)]
#![feature(lang_items)]
#![no_core]

// { dg-output "attempted to zero-initialize type `!`, which is invalid\r*\nattempted to zero-initialize type `Nothing`, which is invalid\r*\n" }

#[lang = "sized"]
trait Sized {}

#[lang = "panic"]
pub fn panic(s: &'static str) -> ! {
    unsafe {
        puts(s as *const str as *const i8);
    }
}

extern "C" {
    fn puts(s: *const i8);
}

extern "rust-intrinsic" {
    fn assert_inhabited<T>();
    fn assert_zero_valid<T>();
    fn assert_uninit_valid<T>();
}

enum Nothing {}

fn foo() {
    unsafe { assert_zero_valid::<!>() }; // { dg-warning "no-op" }
    unsafe { assert_zero_valid::<Nothing>() }; // { dg-warning "no-op" }
    unsafe { assert_zero_valid::<i32>() }; // { dg-warning "no-op" }
}

fn main() -> i32 {
    foo();

    0
}
