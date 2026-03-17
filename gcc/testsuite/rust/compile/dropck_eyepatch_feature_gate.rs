// { dg-options "-frust-compile-until=lowering" }
#![feature(no_core)]
#![no_core]

struct Test<T> {
    _inner: T,
}

trait Action {}

unsafe impl<#[may_dangle] T> Action for Test<T> {} //{ dg-error ".may_dangle. has unstable semantics and may be removed in the future." "" { target *-*-* }  }
