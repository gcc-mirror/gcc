#![feature(no_core)]
#![no_core]

struct Foo {
    x: i32
}

struct Bar (i32, i32);

fn main() -> i32 {
    let mut my_foo = Foo{x: 32};
    let mut my_bar = Bar(32, 64);
    let Foo {x} = my_foo;
    let Foo {x: _x @ test} = my_foo;
    my_foo.x = 16;
    let Foo {x: yet_another_x } = my_foo;
    let Bar {0: w, ..} = my_bar;
    (x - test) | (x - w) | (yet_another_x - 16)
}
