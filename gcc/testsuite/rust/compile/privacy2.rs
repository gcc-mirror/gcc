// { dg-additional-options "-w" }
#![feature(no_core)]
#![no_core]


mod orange {
    fn tangerine() {}

    mod green {
        mod blue {
            use super::super::tangerine;
            fn berry() {
                tangerine();
            }
        }
    }
}
