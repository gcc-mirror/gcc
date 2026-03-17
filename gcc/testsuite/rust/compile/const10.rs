#![feature(no_core)]
#![no_core]

const fn foo (a: &mut i32) { // { dg-error "mutable references are not allowed in constant functions" }
	*a = 1;
}
