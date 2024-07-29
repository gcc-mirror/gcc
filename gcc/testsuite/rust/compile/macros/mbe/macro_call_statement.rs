fn f() {}

macro_rules! panic {
    () => (
        crate::f()
    );
}

fn main() {
    panic!();
}
