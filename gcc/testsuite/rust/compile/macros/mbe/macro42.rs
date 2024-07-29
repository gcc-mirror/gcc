// { dg-additional-options "-w -frust-cfg=A" }
#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! cfg {
    () => {{}};
}

fn main() -> i32 {
    let mut res = 0;
    if cfg!(A) {
        res = 1;
    }

    if cfg!(A) {
        res = 2;
    } else {
        res = 3;
    }

    if cfg!(A) {
        res = 4;
    } else if cfg!(A) {
        res = 5;
    }

    let res = if cfg!(A) {
        6
    } else {
        7
    };

    return res;
}
