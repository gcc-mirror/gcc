trait Foo {
    type Bar;
}

trait Copy {}


fn c<F: Foo<Bar: Foo>>() where F::Bar: Copy { // { dg-warning "function is never used: 'c'" }
}

