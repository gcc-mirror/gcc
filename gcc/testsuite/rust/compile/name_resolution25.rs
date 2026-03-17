#![feature(no_core)]
#![no_core]

struct Test; // { dg-warning "struct is never constructed: .Test." }

impl Test {}
