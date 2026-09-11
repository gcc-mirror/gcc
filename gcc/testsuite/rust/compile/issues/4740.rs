#![feature(no_core)]
#![no_core]

trait A {
    fn foo();
}

// { dg-error "failed to find lang item" "" { target *-*-* } .+1 }
impl A for [(); (|| 1) ()] {}
// { dg-message "terminated" "" { target *-*-* } 0 }
