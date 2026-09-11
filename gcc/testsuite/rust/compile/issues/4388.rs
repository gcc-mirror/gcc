#![feature(no_core)]
#![no_core]

#[export_name] // { dg-error "malformed" }
static A: i32 = 0;

#[export_name(123)] // { dg-error "attribute must be a string literal" }
static B: i32 = 0;

#[export_name = 123] // { dg-error "attribute must be a string literal" }
static C: i32 = 0;

#[export_name = "valid_static"]
static D: i32 = 0;

fn main() {}