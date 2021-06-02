// { dg-xfail-if "pub visibility not supported #432"  *-*-* }

mod foomod {
    pub struct Foo {
        pub f: i32,
        pub g: u32,
    }
}

fn test() -> foomod::Foo {
    foomod::Foo{
        f:1,
        g:3,
    }
}
