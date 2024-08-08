/* { dg-output "parent123\r*\nchild\r*\n" } */

//Second test for lifetimes in supertraits

extern "C" {
    fn printf(s: *const i8, ...);
}
 
struct Foo {
    my_int: u32,
}

trait Parent {
    fn parent(&self);
}

trait Child : Parent {
    fn child(&self);
}

impl Parent for Foo {
    fn parent<'b>(&self) {
        unsafe {
            let parent = "parent%i\n\0";
            let msg = parent as *const str;
            printf(msg as *const i8,self.my_int);
            return;
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
    let a = Foo{ my_int: 123};
    let b: &dyn Child = &a;

    b.parent();
    b.child();

    // Silence bogus warning
    let _ = a.my_int;

    0
}
