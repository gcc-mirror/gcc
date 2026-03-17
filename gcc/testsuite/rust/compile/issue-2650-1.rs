// { dg-additional-options "-frust-edition=2018" }
#![feature(no_core)]
#![no_core]


pub async fn a() -> u32 {
    1
}
