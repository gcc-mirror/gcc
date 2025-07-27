#[lang = "sized"]
trait Sized {}

const M: usize = 4;

struct Foo<T, const N: usize = 1> {
    value: [T; N], // { dg-warning "field is never read: .value." }
}

fn main() {
    let _foo = Foo::<i32> { value: [15] };
    let _foo = Foo::<i32, 2> { value: [15, 13] };
    let _foo: Foo<i32, 2> = Foo { value: [15, 13] };
    let _foo: Foo<i32, 2> = Foo::<i32, 2> { value: [15, 13] };
    let _foo: Foo<i32, { 1 + 1 }> = Foo { value: [15, 13] };
    let _foo = Foo::<i32, { 1 + 1 }> { value: [15, 13] };
    let _foo: Foo<i32, { 1 + 1 }> = Foo::<i32, { 1 + 1 }> { value: [15, 13] };
    let _foo: Foo<i32, M> = Foo::<i32, 4> {
        value: [15, 13, 11, 9],
    };
}
