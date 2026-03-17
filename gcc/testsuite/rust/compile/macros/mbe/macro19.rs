// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]


macro_rules! call_without_semi {
    () => {
        f()
    };
    (block) => {{
        f()
    }};
}

fn f() {}

fn main() -> i32 {
    call_without_semi!();
    call_without_semi!(block);

    0
}
