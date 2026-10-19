// { dg-options "-w" }

#![feature(no_core)]
#![no_core]

struct S(&'static T);
struct T(&'static S);
