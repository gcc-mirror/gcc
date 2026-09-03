#![feature(no_core)]
#![no_core]
#![feature(used_with_arg)]

#[used]
static STAT0: i32 = 15;

#[used(compiler)]
static STAT1: i32 = 15;

#[used(linker)]
static STAT2: i32 = 15;

#[used(assembler)] // { dg-error "malformed .used. attribute input" }
static STAT3: i32 = 15;
