// { dg-output "Foo::bar\r*\n" }
// { dg-additional-options "-w" }
extern "C" {
    fn printf(s: *const i8, ...);
}

struct Foo {}

trait Bar {
    fn bar(&self) {
        unsafe {
            let _a = "Bar::bar\n\0";
            let _b = _a as *const str;
            let _c = _b as *const i8;
            printf(_c);
        }
    }
}

impl Foo {
    fn bar(&self) {
        unsafe {
            let _a = "Foo::bar\n\0";
            let _b = _a as *const str;
            let _c = _b as *const i8;
            printf(_c);
        }
    }
}

impl Bar for Foo {
    fn bar(&self) {
        unsafe {
            let _a = "<Bar as Foo>::bar\n\0";
            let _b = _a as *const str;
            let _c = _b as *const i8;
            printf(_c);
        }
    }
}

pub fn main() -> i32 {
    let mut f = Foo {};
    f.bar();

    0
}
