mod A {
    pub mod B {  // { dg-warning "unused name" }
        pub mod C { // { dg-warning "unused name" }
            pub struct Foo {
                pub f: i32,
            }
            impl Foo {
                pub fn new() -> Self {  // { dg-warning "unused name" }
                    Foo {
                        f: 23i32,
                    }
                }
            }
        }
    }
}

fn main() ->i32 {
    let a = A::B::C::Foo::new();
    let b = A::B::C::Foo {
        f: -23i32,
    };

    a.f + b.f
}
