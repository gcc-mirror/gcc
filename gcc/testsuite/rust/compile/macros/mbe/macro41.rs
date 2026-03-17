#![feature(no_core)]
#![no_core]

macro_rules! empty {
    ($($t:tt)*) => {};
}

empty! {nothing}
empty! {struct OuterItem;}
empty! {}

fn main() {
    empty! {as statement};
    empty! {any child item};
    empty! {};
}
