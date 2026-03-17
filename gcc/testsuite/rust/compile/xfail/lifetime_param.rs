// { dg-excess-errors "warnings" }
#![feature(no_core)]
#![no_core]


// { dg-error "lifetime not defined" "#359" { xfail *-*-* } .+1 }
fn lifetime_undefined(t: &'a str) -> &'a str {
    t
}

// { dg-error "lifetime not defined" "#359" { xfail *-*-* } .+1 }
fn lifetime_undefined_bis<'a>(t: &'a str)-> &'b str {
    t
}
