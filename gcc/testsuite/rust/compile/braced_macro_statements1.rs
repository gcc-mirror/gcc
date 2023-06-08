macro_rules! m {
    () => { bar() }
}

fn bar () {}

fn foo() {
    m!{}
    m!{}
}

fn main() -> i32 {
    foo();
    0
}
