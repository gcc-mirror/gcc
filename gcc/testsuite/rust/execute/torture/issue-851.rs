/* { dg-output "Result: 123\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

enum Foo<T> {
    A,
    B(T),
}

fn inspect(a: Foo<i32>) {
    match a {
        Foo::A => unsafe {
            let a = "A\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        },
        Foo::B(x) => unsafe {
            let a = "Result: %i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, x);
        },
    }
}

fn main() -> i32 {
    let a = Foo::B(123);
    inspect(a);

    0
}
