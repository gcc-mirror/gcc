#![feature(no_core, extern_types)]
#![no_core]

extern "C" {
    type X;
}

type Y = X;
