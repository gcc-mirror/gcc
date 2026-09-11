#![feature(no_core)]
#![no_core]

// { dg-error "undeclared lang item: .*into_iter.*" "" { target *-*-* } 0 }

fn main() {
    'inner: while let Some(0u32) = None {
    // { dg-error "Cannot find path .None. in this scope" "" { target *-*-* } .-1 }
    // { dg-error "Cannot find path .Some. in this scope" "" { target *-*-* } .-2 }

        let _ = Iterator::next(&mut ());
        // { dg-error "Cannot find path .Iterator::next. in this scope" "" { target *-*-* } .-1 }

        let _ = Iterator::next(&mut ());
        // { dg-error "Cannot find path .Iterator::next. in this scope" "" { target *-*-* } .-1 }
        for _ in false {}
    }
}