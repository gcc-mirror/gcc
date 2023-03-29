// { dg-options "-w" }
extern "C" {
    fn printf(s: *const i8, ...);
}

enum Foo {
    A,
    B(i32),
}

fn main() {
    let result = Foo::B(123);

    match result {
        A => unsafe {
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
