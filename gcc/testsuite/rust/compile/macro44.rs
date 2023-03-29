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

    fn foo_f() { // { dg-warning "function is never used" }
        foo!();
    }

    fn bar_f() { // { dg-warning "function is never used" }
        baz!();
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
