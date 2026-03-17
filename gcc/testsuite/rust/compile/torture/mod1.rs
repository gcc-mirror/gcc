// This is testing name resolution
#![feature(no_core)]
#![no_core]


mod _foo {
    struct _A;
}

mod _bar {
    mod _barbis {
        struct _B;
    }
}
