fn foo() -> bool {
    true
}

fn bar() -> bool {
    false
}

struct Foo1 {
    one: i32
}


fn main() {
    if foo() {
    } else if bar() {
        let a = Foo1{one: 1};
        a.one;
    }
}