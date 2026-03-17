#![feature(no_core)]
#![no_core]

use self;
// { dg-error ".self. imports are only allowed within a { } list" "" { target *-*-* } .-1 }
