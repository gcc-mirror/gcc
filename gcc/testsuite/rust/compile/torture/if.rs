fn foo() -> bool {
    true
}

fn bar() {}

fn baz(a: i32) {
    a;
}

struct Foo1 {
    one: i32
}


fn main() {
    if foo() {
        bar();
        let a = Foo1{one: 1};
        baz (a.one);
    }

}
