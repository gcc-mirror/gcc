// { dg-options "-fsyntax-only" }

#![feature(dropck_eyepatch)]
struct Test<T> {
    _inner: T,
}

trait Action {}

unsafe impl<#[may_dangle] T> Action for Test<T> {}
