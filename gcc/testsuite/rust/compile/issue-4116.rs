#![feature(no_core)]
#![no_core]

enum Foo {
    Relaxed,
    SeqCst,
    StressedOut,
}

use Foo::{Relaxed, SeqCst, StressedOut};

fn main() {
    let a = (Relaxed, Relaxed);

    match a {
        (Relaxed, Relaxed) => {}
        _ => {}
    }
}
