/* { dg-output "child\r*\n" }*/
extern "C" {
    fn printf(s: *const i8, ...);
}

#[lang = "sized"]
pub trait Sized {}

struct Foo {
    my_int: u32,
    // { dg-warning "field is never read: .my_int." "" { target *-*-* } .-1 }
}

trait Parent<T> {
    fn parent(&self) -> T;
}

trait Child: Parent<u32> {
    fn child(&self);
}

impl Parent<u32> for Foo {
    fn parent(&self) -> u32 {
        unsafe {
            let parent = "parent %i\n\0";
            let msg = parent as *const str;
            printf(msg as *const i8, self.my_int);
            return self.my_int;
        }
    }
}

impl Child for Foo {
    fn child(&self) {
        let _ = self;
        unsafe {
            let child = "child\n\0";
            let msg = child as *const str;
            printf(msg as *const i8);
        }
    }
}

pub fn main() -> i32 {
    let a = Foo { my_int: 0xf00dfeed };
    let b: &dyn Child = &a;

    // b.parent();
    b.child();

    0
}
