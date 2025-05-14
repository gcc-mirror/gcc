// { dg-additional-options "-frust-compile-until=typecheck" }

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
    fn a_method(&self) {}
}

fn main() {
    let s = S;

    foo(&s);
}
