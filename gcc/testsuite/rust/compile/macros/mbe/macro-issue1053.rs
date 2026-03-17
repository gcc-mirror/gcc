#![feature(no_core)]
#![no_core]

macro_rules! m {
    ($e:expr $(,)*) => {{}};
}
