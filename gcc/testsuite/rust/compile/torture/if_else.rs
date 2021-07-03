fn foo() -> bool {
    true
}

fn bar() {}

struct Foo1 {
    one: i32
}


fn main() {
    if foo() {
        bar();
    } else {
        let a = Foo1{one: 1};
        a.one;
    }
}