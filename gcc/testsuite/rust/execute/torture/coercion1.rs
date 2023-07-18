/* { dg-output "123\r*\n123\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

struct Foo(i32);
trait Bar {
    fn baz(&self);
}

impl Bar for Foo {
    fn baz(&self) {
        unsafe {
            let a = "%i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, self.0);
        }
    }
}

fn static_dispatch<T: Bar>(t: &T) {
    t.baz();
}

fn dynamic_dispatch(t: &dyn Bar) {
    t.baz();
}

fn main() -> i32 {
    let a;
    a = Foo(123);
    static_dispatch(&a);

    let b: &dyn Bar;
    b = &a;
    dynamic_dispatch(b);

    0
}
