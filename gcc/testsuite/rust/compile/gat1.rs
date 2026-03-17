#![feature(no_core)]
#![no_core]

trait Foo {
    type Bar<T>;
    type Baz<'a>;
}
