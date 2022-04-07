// { dg-additional-options "-fdump-tree-gimple -frust-crate=example" }

struct Foo {
    a: u16,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
    b: u8,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn main() {
    let my_foo = Foo { a: 1, b: 2 };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    // { dg-final { scan-tree-dump-times {(?n)const struct example::Foo my_foo;$} 1 gimple } }
}
