// { dg-output "x:15\r*\ny:20\r*\n" }

extern "C" {
    fn printf(s: *const i8, ...);
}

enum Foo {
    A,
    B,
}

fn inspect(f: Foo, g: u8) -> i32 {
    match (f, g) {
        (Foo::A, 1) => {
            return 5;
        }

        (Foo::A, 2) => {
            return 10;
        }

        (Foo::B, 2) => {
            return 15;
        }

        _ => {
            return 20;
        }
    }
    return 25;
}

fn main() -> i32 {
    let x = inspect(Foo::B, 2);
    let y = inspect(Foo::B, 1);

    unsafe {
        printf("x:%d\n" as *const str as *const i8, x);
    }
    unsafe {
        printf("y:%d\n" as *const str as *const i8, y);
    }

    y - x - 5
}
