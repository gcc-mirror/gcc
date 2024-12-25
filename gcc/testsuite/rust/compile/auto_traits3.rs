#![feature(optin_builtin_traits)]

pub unsafe auto trait Send {}
#[lang = "sync"]
pub unsafe auto trait Sync {}

trait A {
    fn a_method(&self) {}
}

fn foo(a: &(dyn A + Send + Sync)) {
    a.a_method();
}

struct S;

impl A for S {
    fn a_method(&self) {} // { dg-warning "unused" }
}

// These should not be necessary because they are both auto traits
// They need to be removed once we figure out the proper implementation for each of them
// However, it'd be silly to implement other traits in order to ensure the test is okay,
// as these extra trait bounds are only allowed to use auto traits
// FIXME: #3327
// FIXME: #3326
unsafe impl Send for S {}
unsafe impl Sync for S {}

fn main() {
    let s = S;

    foo(&s);
}
