#![feature(no_core, lang_items, optin_builtin_traits, negative_impls)]
#![no_core]

// An auto trait bound on nested structs is still checked through every
// field, so a negative impl deep inside makes the outer struct fail it.

#[lang = "sized"]
pub trait Sized {}

pub unsafe auto trait Send {}
pub unsafe auto trait Sync {}

pub struct Ok;
pub struct NotSend;
impl !Send for NotSend {}

pub struct A1(Ok, Ok);
pub struct A2(A1, A1);
pub struct A3(A2, A2);
pub struct A4(A3, A3);

pub struct B1(Ok, NotSend);
pub struct B2(A1, B1);
pub struct B3(A2, B2);
pub struct B4(A3, B3);

fn need_send<X: Send>(_x: X) {}

pub fn ok(x: A4) {
    need_send(x);
}

pub fn not_send(x: B4) {
    need_send(x); // { dg-error "bounds not satisfied for B4 .Send. is not satisfied .E0277." }
}
