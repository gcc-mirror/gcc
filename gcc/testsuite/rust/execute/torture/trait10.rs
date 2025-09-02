/* { dg-output "123\r*\n" } */
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

struct S;
impl S {
    fn dynamic_dispatch(self, t: &dyn Bar) {
        t.baz();
    }
}

pub fn main() -> i32 {
    let a;
    a = &Foo(123);

    let b;
    b = S;

    b.dynamic_dispatch(a);

    0
}
