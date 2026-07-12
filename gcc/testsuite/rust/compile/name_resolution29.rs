// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

mod a {
    mod b {
        pub (in super::super) struct S;
        pub (in self::super::super) struct T;
    }
}
