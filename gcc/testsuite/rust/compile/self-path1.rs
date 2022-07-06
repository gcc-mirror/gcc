// { dg-additional-options "-w" }
struct foo;

fn bar() -> self::foo {
    crate::foo
}

fn baz() {
    let a: foo = self::bar();

    crate::bar();
}
