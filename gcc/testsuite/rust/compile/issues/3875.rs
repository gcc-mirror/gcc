#![crate_type = "lib"]
#![feature(no_core)]
#![feature(lang_items)]
#![no_core]
// { dg-options "-w" }

#[lang = "sized"]
pub trait Sized {}

#[lang = "clone"]
pub trait Clone: Sized {
    fn clone(&self) -> Self;
}

#[lang = "copy"]
pub trait Copy: Clone {}

#[derive(Clone)] // { dg-error "derive may only be applied to structs, enums and unions" }
fn foo() {}

#[derive(Clone)] // { dg-error "derive may only be applied to structs, enums and unions" }
trait Bar {}

// Valid test: Derive Copy and Clone on a unit struct
#[derive(Copy, Clone)]
struct TupleStruct;

// { dg-prune-output "could not resolve" }