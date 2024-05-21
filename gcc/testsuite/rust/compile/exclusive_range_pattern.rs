// { dg-options "-fsyntax-only" }
#![feature(exclusive_range_pattern)]

const TEN: usize = 10;

fn test(n: usize) {
    match n {
        0..TEN => (),
        TEN..20 => (),
        20..30 => (),
        _ => (),
    }
}

fn test2(t: usize) {
    if let 0..10 = t {}
}
