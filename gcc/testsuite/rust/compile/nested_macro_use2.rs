#[macro_use]
mod foo {
    fn bar() {
        macro_rules! baz {
            () => {{}};
        }
    }
}

fn main() {
    baz!(); // { dg-error "could not resolve macro invocation .baz." }
}
