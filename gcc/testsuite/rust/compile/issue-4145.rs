// { dg-excess-errors "warnings" }

struct S {
    field: [u8; {
        #[path = "outer/inner.rs"]
        // { dg-warning "error handling module file for .inner." "#4145" { xfail *-*-* } .+1 }
        mod inner;
        // OK
        0
    }],
}

fn main() {}
