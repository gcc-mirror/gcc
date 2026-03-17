#![feature(no_core)]
#![no_core]

fn foo() {
    (""; // { dg-error "unexpected token .*" }
}
