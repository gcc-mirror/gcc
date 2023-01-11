/* { dg-output "Result: 123\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

enum Foo<T> {
    A,
    B(T),
}

fn main() -> i32 {
    let result = Foo::B(123);

    match result {
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

    0
}
