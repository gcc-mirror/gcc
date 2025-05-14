#![feature(dropck_eyepatch)]
#[allow(dead_code)]

#[lang = "sized"]
trait Sized {}

struct Test<T> {
    _inner: T,
}

struct Test2<T> {
    _inner: T,
}

trait Action {}

impl<#[may_dangle] T> Action for Test<T> {} // { dg-error "use of 'may_dangle' is unsafe and requires unsafe impl" "" { target *-*-* } 0 }

unsafe impl<#[may_dangle] T> Action for Test2<T> {}

fn main() {}
