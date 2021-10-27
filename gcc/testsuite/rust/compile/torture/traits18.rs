trait Foo<'a> {}

trait Bar {
    // { dg-warning "unused name .Bar." "" { target *-*-* } .-1 }

    type Item: for<'a> Foo<'a>;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
