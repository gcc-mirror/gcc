#![feature(no_core)]
#![no_core]

// Rust-GCC/gccrs#2311: unit variants in patterns used to be treated as
// wildcards, so this match was accepted.

enum Terminator {
    HastaLaVistaBaby,
    TalkToMyHand,
}

fn main() {
    let x = Terminator::HastaLaVistaBaby;

    match x {
        // { dg-error "non-exhaustive patterns: 'Terminator::HastaLaVistaBaby' not covered" "" { target *-*-* } .-1 }
        Terminator::TalkToMyHand => {}
    }
}
