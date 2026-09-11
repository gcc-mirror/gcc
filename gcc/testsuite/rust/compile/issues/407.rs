// This already worked before the #409 code changes.
#![feature(no_core)]
#![no_core]

fn test()  {
    let mut a = 1;
    a + = 1; // { dg-error "found unexpected token '=' in null denotation" }
}
