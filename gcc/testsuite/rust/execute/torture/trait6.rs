/* { dg-output "123\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

pub trait Foo {
    type A;

    fn bar(self) -> Self::A;
}

struct S(i32);
impl Foo for S {
    type A = i32;

    fn bar(self) -> Self::A {
        self.0
    }
}

fn test_bar<T: Foo>(x: T) -> T::A {
    x.bar()
}

fn main() -> i32 {
    let a;
    a = S(123);

    let bar: i32 = test_bar::<S>(a);
    unsafe {
        let a = "%i\n\0";
        let b = a as *const str;
        let c = b as *const i8;

        printf(c, bar);
    }

    0
}
