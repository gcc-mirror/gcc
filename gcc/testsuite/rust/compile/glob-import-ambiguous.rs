#![feature(no_core)]
#![no_core]

mod a {
    pub fn x() {}
    pub fn y() {}
}

mod b {
    pub fn x() {}
    pub fn z() {}
}

mod c {
    pub use crate::a::*;
    pub use crate::b::*;
}

pub fn main() -> i32 {
    use crate::c::*;
    a::x();
    b::x();
    y();
    z();
    0
}
