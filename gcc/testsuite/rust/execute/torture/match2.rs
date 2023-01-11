// { dg-output "123\r*\n80\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

enum Foo {
    C(i32),
    D { x: i32, y: i32 },
}

fn inspect(f: Foo) -> i32 {
    match f {
        Foo::C(x) => x,
        Foo::D { x, y } => y,
    }
}

fn main() -> i32 {
    let a = Foo::C(123);
    let b = Foo::D { x: 20, y: 80 };

    let result = inspect(a);
    unsafe {
        let a = "%i\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, result);
    }

    let result = inspect(b);
    unsafe {
        let a = "%i\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, result);
    }

    0
}
