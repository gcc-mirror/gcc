#[lang = "sized"]
pub trait Sized {}

pub fn main() {
    let a;
    a = foo { a: 123, b: 456f32 };

    let mut a = 123;
    a = bar(a);

    let mut b = 456f32;
    b = bar(b);

    fn bar<T>(x: T) -> T {
        x
    }

    struct foo {
        a: i32,
        b: f32,
    };
}
