#![feature(no_core)]
#![no_core]

const A: u8 = 255 + 1;
// { dg-error {attempt to compute .255 \+ 1., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

const B: u8 = 0 - 1;
// { dg-error {attempt to compute .0 - 1., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

const C: u8 = 128 * 2;
// { dg-error {attempt to compute .128 \* 2., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

const D: u64 = 18446744073709551615 + 1;
// { dg-error {attempt to compute .18446744073709551615 \+ 1., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

// The operands of a named constant are evaluated before the operation, so the
// overflow is still reported against the operation itself.
const E: u8 = 255;
const F: u8 = E + 1;
// { dg-error {attempt to compute .255 \+ 1., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

static G: u8 = 255 + 1;
// { dg-error {attempt to compute .255 \+ 1., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

// Does not overflow, must keep compiling.
const H: u8 = 200 + 55;

// Wrapping at run time is not a constant evaluation, nothing to diagnose here.
fn runtime (x: u8, y: u8) -> u8 {
  x + y
}
