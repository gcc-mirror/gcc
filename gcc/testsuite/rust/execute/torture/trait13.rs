/* { dg-output "123\r*\n456\r*\n" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

struct Foo(i32);
trait Bar {
    fn baz(&self);

    fn qux(&self) {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        unsafe {
            let a = "%i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, 456);
        }
    }
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

fn dynamic_dispatch(t: &dyn Bar) {
    t.baz();
    t.qux();
}

fn main() -> i32 {
    let a;
    a = Foo(123);

    let b: &dyn Bar;
    b = &a;
    dynamic_dispatch(b);

    0
}
