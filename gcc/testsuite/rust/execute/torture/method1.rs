/* { dg-output "124\r*\n458" } */
extern "C" {
    fn printf(s: *const i8, ...);
}

struct Foo(i32);
impl Foo {
    fn bar(&self, i: i32) {
        unsafe {
            let a = "%i\n\0";
            let b = a as *const str;
            let c = b as *const i8;

            printf(c, self.0 + i);
        }
    }
}

fn main() -> i32 {
    let a = Foo(123);
    a.bar(1);

    let b = &Foo(456);
    b.bar(2);

    0
}
