/* { dg-output "parent\r*\nchild\r*\n" } */
//Testing default implementations with supertraits.

extern "C" {
    fn printf(s: *const i8, ...);
}
 
struct Foo {
    my_int: u32,
}

trait Parent {
    fn parent_str(&self) -> &'static str;
    fn parent(&self) {
        unsafe {
            let parent: &'static str = self.parent_str();
            let msg = parent as *const str;
            printf(msg as *const i8);
        }
    }
}

trait Child : Parent {
    fn child(&self);
}

impl Parent for Foo {
    fn parent_str(&self) -> &'static str {
        let _ = self;
        return "parent\n\0";
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
    let a = Foo{ my_int: 0xfeedf00d};
    let b: &dyn Child = &a;

    b.parent();
    b.child();

    // Bogus warning silencer
    let _ = a.my_int;

    0
}
