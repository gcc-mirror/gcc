// { dg-additional-options "-w -frust-name-resolution-2.0" }

#[lang = "sized"]
trait Sized {}

const M: usize = 4;

struct Foo<T, const N: usize = 1> {
    value: [T; N],
}

fn main() {
    let foo = Foo::<i32> { value: [15] };
    let foo = Foo::<i32, 2> { value: [15, 13] };
    let foo: Foo<i32, 2> = Foo { value: [15, 13] };
    let foo: Foo<i32, 2> = Foo::<i32, 2> { value: [15, 13] };
    let foo: Foo<i32, { 1 + 1 }> = Foo { value: [15, 13] };
    let foo = Foo::<i32, { 1 + 1 }> { value: [15, 13] };
    let foo: Foo<i32, { 1 + 1 }> = Foo::<i32, { 1 + 1 }> { value: [15, 13] };
    let foo: Foo<i32, M> = Foo::<i32, 4> {
        value: [15, 13, 11, 9],
    };

    // FIXME: Add proper const typecheck errors here
    let invalid_foo: Foo<i32, { 1 + 1 }> = Foo::<i32, 3> { value: [15, 13] };
    let invalid_foo: Foo<i32, { 1 + 1 }> = Foo::<i32, M> { value: [15, 13] };
    let invalid_foo: Foo<i32> = Foo::<i32, 2> { value: [15, 13] };
}
