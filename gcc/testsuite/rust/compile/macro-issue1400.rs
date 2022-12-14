// { dg-additional-options "-w" }

macro_rules! foo {
    ( ( $( $Trait: ident ),+ ) for $Ty: ident ) => {
        $(
            impl $Trait for $Ty {
                fn bar() -> i32 {
                    14
                }
            }
        )+
    }
}

trait Foo {
    fn bar() -> i32;
}

trait Bar {
    fn bar() -> i32;
}

trait Baz {
    fn bar() -> i32;
}

trait Qux {
    fn bar() -> i32;
}

struct S;

foo! {(Foo, Bar, Baz, Qux) for S}
