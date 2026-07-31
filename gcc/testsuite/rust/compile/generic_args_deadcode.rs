// Test for issue #4585.
// A type used only as a generic argument must not trigger the
// "struct is never constructed" warning.
#![feature(no_core)]
#![feature(lang_items)]
#![no_core]
#[lang = "sized"]
trait Sized {}
struct GenericArgType;
fn anything<T>() {}
fn main() {
    anything::<GenericArgType>();
}
struct NeverUsed; // { dg-warning "struct is never constructed" }
