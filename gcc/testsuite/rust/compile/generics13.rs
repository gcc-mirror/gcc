struct Foo<A, 'a>; // { dg-error "invalid order for generic parameters: lifetimes should always come before types" }
