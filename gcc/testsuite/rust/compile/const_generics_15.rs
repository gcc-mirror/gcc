#[lang = "sized"]
trait Sized {}

enum Foo<const N: usize> {
    A([u8; N]),
}

union Bar<const N: usize> {
    a: [i32; N],
    b: [u8; N],
}

fn main() {
    let _ = Foo::<4>::A([1, 2, 3, 4]);
    let _ = Bar::<4> { a: [0; 4] };
}
