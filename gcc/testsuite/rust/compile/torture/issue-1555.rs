extern "C" {
    fn printf(s: *const i8, ...);
}

struct Foo(i32);
trait Bar {
    fn baz(&self);
}

trait Baz: Bar {
    fn qux(&self);
}

impl Bar for Foo {
    fn baz(&self) {
        unsafe {
            let a = "baz %i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, self.0);
        }
    }
}

impl Baz for Foo {
    fn qux(&self) {
        unsafe {
            let a = "qux %i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, self.0);
        }
    }
}

fn static_dispatch<T: Baz>(t: &T) {
    t.baz();
    t.qux();
}

pub fn main() {
    let a;
    a = &Foo(123);

    static_dispatch(a);
}
