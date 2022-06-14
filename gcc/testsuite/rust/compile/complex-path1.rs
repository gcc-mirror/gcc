// { dg-additional-options "-w" }
mod a {
    pub fn foo() {}
}

mod b {
    pub fn foo() {
        super::a::foo();
    }
}

mod foo {
    pub struct bar(pub i32);
}

fn test() -> crate::foo::bar {
    foo::bar(123)
}
