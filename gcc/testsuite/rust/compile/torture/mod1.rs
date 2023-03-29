// This is testing name resolution

mod _foo {
    struct _A;
}

mod _bar {
    mod _barbis {
        struct _B;
    }
}
