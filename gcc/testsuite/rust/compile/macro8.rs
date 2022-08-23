fn f() {}

macro_rules! expr {
    ($($a:expr)?) => {
        f();
    };
}

fn main() {
    expr!();
    expr!(14);
}
