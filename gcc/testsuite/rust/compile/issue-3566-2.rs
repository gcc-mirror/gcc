// run-pass

#![allow(H8)]
#![allow(dead_code)]


// pretty-expanded FIXME #23616

mod a {
    pub mod b {
        pub type t = isize;

        pub fn f(x: [u8; { let s = 17; 100 }]) -> [u8;  { let z = 18; 100 }] {
    //~^ WARN unused variable: `s`
    //~| WARN unused variable: `z`
    x
}
    }
}

pub fn main() {    //~ ERROR cannot move out
    }
