mod foomod {
    pub struct Foo {}
}

impl foomod::Foo {
    pub fn new() -> Self {
        foomod::Foo {}
    }
}

fn main() {
    let _a = foomod::Foo::new();
}
