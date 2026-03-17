#![feature(no_core)]
#![no_core]

#[target_feature(sse)]
unsafe fn foo() {
    let a: usize = 0;
}

fn main() {
    foo(); // { dg-error "requires unsafe function or block" }
    unsafe { foo(); }
}
