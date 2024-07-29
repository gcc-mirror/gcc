mod foo {
    mod bar {
        mod baz {
            macro_rules! baz {
                () => {{}};
            }
        }
    }

    macro_rules! foo {
        () => {{}};
    }

    fn foo_f() {
        foo!();
    }

    fn bar_f() {
        baz!(); // { dg-error "unknown macro" }
    }
}

mod foo2 {
    #[macro_export]
    macro_rules! bar1 {
        () => {};
    }

    macro_rules! bar2 {
        () => {};
    }
}

fn main() {}
