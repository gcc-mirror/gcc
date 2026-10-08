#![feature(no_core)]
#![no_core]

// A rest pattern in an enum tuple variant used to ICE in the backend.

pub enum E {
    A(i32, i32, i32),
    B,
}

pub fn first(e: E) -> i32 {
    match e {
        E::A(x, ..) => x,
        E::B => 0,
    }
}

pub fn last(e: E) -> i32 {
    match e {
        E::A(.., y) => y,
        E::B => 0,
    }
}

pub fn ends(e: E) -> i32 {
    match e {
        E::A(x, .., y) => x + y,
        E::B => 0,
    }
}

pub fn any(e: E) -> bool {
    match e {
        E::A(..) => true,
        E::B => false,
    }
}
