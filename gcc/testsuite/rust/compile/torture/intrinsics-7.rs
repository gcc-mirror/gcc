#![feature(intrinsics)]

#[lang = "sized"]
pub trait Sized {}

extern "rust-intrinsic" {
    pub fn unchecked_add<T>(x: T, y: T) -> T;
    // { dg-error "unchecked operation intrinsics can only be used with basic integer types .got .NotAdd.." "" { target *-*-* } .-1 }
}

fn main() {
    struct NotAdd;

    unsafe { unchecked_add(NotAdd, NotAdd) };
}
