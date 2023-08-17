struct Foo<A, 'a>; // { dg-error "invalid order for generic parameters: lifetime parameters must be declared prior to type and const parameters" }
