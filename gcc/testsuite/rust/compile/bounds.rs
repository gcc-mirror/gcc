// { dg-options "-fsyntax-only" }
trait Foo {
    type Bar;
}

trait Copy {}

fn c<F: Foo<Bar: Foo>>()
where
    F::Bar: Copy,
{
}
