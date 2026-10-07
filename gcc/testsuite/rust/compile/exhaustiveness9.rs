#![feature(no_core)]
#![no_core]

// Unit variants used to be treated as wildcards, so any match on them looked
// exhaustive.

enum E {
    A,
    B,
}

enum Three {
    A,
    B,
    C,
}

enum Mixed {
    A,
    B(i32),
    C,
}

const C: E = E::A;

fn missing_arm(e: E) {
    match e {
        // { dg-error "non-exhaustive patterns: 'E::B' not covered" "" { target *-*-* } .-1 }
        E::A => {}
    }
}

fn mixed(m: Mixed) {
    match m {
        // { dg-error "non-exhaustive patterns: 'Mixed::C' not covered" "" { target *-*-* } .-1 }
        Mixed::A => {}
        Mixed::B(_) => {}
    }
}

// These are valid matches
fn all_covered(e: E) {
    match e {
        E::A => {}
        E::B => {}
    }
}

fn with_wildcard(t: Three) {
    match t {
        Three::A => {}
        _ => {}
    }
}

fn through_reference(e: &E) {
    match e {
        &E::A => {}
        &E::B => {}
    }
}

fn const_arm(e: E) {
    match e {
        C => {}
        E::B => {}
    }
}

fn main() {}
