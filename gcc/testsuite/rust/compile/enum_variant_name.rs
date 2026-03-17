// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]

struct E1;

enum Test {
    E1 = {
        let x = E1;
        {
            let x = E1;
        }
        0
    },
}
