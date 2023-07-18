/* { dg-output "Hello World 123\r*\n" }*/
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

struct Foo<T>(T);

struct Bar<T> {
    a: Foo<T>,
    b: bool,
    // { dg-warning "field is never read" "" { target *-*-* } .-1 }
}

fn test<T>(a: Bar<T>) -> Foo<T> {
    a.a
}

fn main() -> i32 {
    let a: Bar<i32> = Bar::<i32> {
        a: Foo::<i32>(123),
        b: true,
    };
    let result: Foo<i32> = test(a);

    unsafe {
        let a = "Hello World %i\n";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, result.0);
    }
    0
}
