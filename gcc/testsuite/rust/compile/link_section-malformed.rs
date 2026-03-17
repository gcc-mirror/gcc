// { dg-options "-w" }
#![feature(no_core)]
#![no_core]

#[link_section] // { dg-error "malformed .link_section. attribute input" }
pub static VAR1: u32 = 1;

// { dg-note "must be of the form" "" { target *-*-* } .-3 }
