// { dg-additional-options "-w -frust-compile-until=lowering" }
#![feature(no_core, trait_alias)]
#![no_core]

trait A {}

trait B = A;
