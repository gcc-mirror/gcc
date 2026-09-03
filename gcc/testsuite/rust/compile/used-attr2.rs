#![feature(no_core)]
#![no_core]

#[used]
static STAT0: i32 = 15;

#[used(compiler)] // { dg-error "malformed ..used. attribute input" }
static STAT1: i32 = 15;

#[used(linker)] // { dg-error "malformed ..used. attribute input" }
static STAT2: i32 = 15;
