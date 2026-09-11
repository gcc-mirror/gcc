#![feature(no_core)]
#![no_core]

#[export_name] // { dg-error "malformed" }
fn foo() {}

#[export_name(123)] // { dg-error "attribute must be a string literal" }
fn bar() {}

#[export_name = 123] // { dg-error "attribute must be a string literal" }
fn baz() {}

#[export_name = "valid"]
fn qux() {}