// { dg-output "Foo::A\r*\nwildcard\r*\nwildcard\r*\nFoo::D 20 80\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

enum Foo {
    A,
    B,
    C(char),
    D { x: i32, y: i32 },
}

fn inspect(f: Foo) {
    match f {
        Foo::A => unsafe {
            let a = "Foo::A\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        },
        Foo::D { x, y } => unsafe {
            let a = "Foo::D %i %i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, x, y);
        },
        _ => unsafe {
            let a = "wildcard\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        },
    }
}

fn main() -> i32 {
    let a = Foo::A;
    let b = Foo::B;
    let c = Foo::C('x');
    let d = Foo::D { x: 20, y: 80 };

    inspect(a);
    inspect(b);
    inspect(c);
    inspect(d);

    0
}
