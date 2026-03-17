#![feature(no_core)]
#![no_core]

macro_rules! check {
    (a, b, c ; x, y, z; e, r; a) => {}
}

macro_rules! foo {
    ($($($i:ident),*);*) => {check!($($($i),*);*);}
}

foo!(a, b, c ; x, y, z; e, r; a);
