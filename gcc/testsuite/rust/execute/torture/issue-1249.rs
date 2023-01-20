// { dg-options "-w" }
// { dg-output "1\r*\n2\r*\n" }

extern "C" {
    fn printf(s: *const i8, ...);
}

trait T {
    fn foo(&self);
}

impl dyn T {
    fn bar(&self) {
        unsafe {
            let a = "1\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            printf(c);
        }
        self.foo()
    }
}

struct S;
impl T for S {
    fn foo(&self) {
        unsafe {
            let a = "2\n\0";
            let b = a as *const str;
            let c = b as *const i8;
            printf(c);
        }
    }
}

pub fn main() -> i32 {
    <dyn T>::bar(&S);
    0
}
