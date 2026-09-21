#![feature(no_core)]
#![no_core]

const A: i8 = 127 + 1;
// { dg-error {attempt to compute .127 \+ 1., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

const B: i8 = -128 - 1;
// { dg-error {attempt to compute .-128 - 1., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

const C: i8 = 64 * 2;
// { dg-error {attempt to compute .64 \* 2., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

const D: i64 = 9223372036854775807 + 1;
// { dg-error {attempt to compute .9223372036854775807 \+ 1., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

const E: i8 = 127;
const F: i8 = E + 1;
// { dg-error {attempt to compute .127 \+ 1., which would overflow \[E0080\]} "" { target *-*-* } .-1 }

// Does not overflow, must keep compiling.
const G: i8 = 100 + 27;
