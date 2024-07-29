fn main() {
    macro_rules! create_type {
        ($s:ident) => {
            struct $s(i32);
        };
    }

    create_type!(Wrapper);

    let _ = Wrapper(15);
}
