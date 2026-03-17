#![feature(no_core)]
#![no_core]

enum E {
    One,
    Two,
    Other
}

fn foo (x: E) -> i32 {
    let mut y = 0;

    match x {
        E::One => {
            y = 1, // { dg-error "expected .;. or .\}. after expression, found .,." "" {target *-*-*} 0 }
        }
        E::Two => {
            y = 2;
        }
        _ => {}
    }

    return y;
}

fn main () {
    let x = E::One;
    let y = foo (x);
}
