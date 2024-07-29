macro_rules! print {
    () => {
        fn puts(s: *const i8);
        fn printf(fmt: *const i8, ...);
    };
}

extern "C" {
    print! {}
}
