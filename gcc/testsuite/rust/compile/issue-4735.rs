#![feature(no_core)]
#![no_core]
use self::self;
// { dg-error ".self. imports are only allowed within a" {} { target *-*-* } .-1 }
