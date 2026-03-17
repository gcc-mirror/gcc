// { dg-additional-options "-frust-compile-until=astvalidation" }
#![feature(no_core)]
#![no_core]

fn foo_1(&self);
fn foo_2(&mut self);
fn foo_3(self);
