#![feature(no_core)]
#![feature(used_with_arg)]
#![no_core]

#[used]
static FOO0: i32 = 15;
// { dg-final { scan-assembler "FOO0" } }

#[used(compiler)]
static FOO1: i32 = 15;
// { dg-final { scan-assembler "FOO1" } }

#[used(linker)]
static FOO2: i32 = 15;
// { dg-final { scan-assembler "FOO2" } }
// { dg-final { scan-assembler ".section.*awR" { target gnu_retain } } }
