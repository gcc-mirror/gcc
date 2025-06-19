/* { dg-output "called Foo::print\\(\\)\r*" } */
/* { dg-options "-w" } */

#[lang = "sized"]
trait Sized {}

trait Printable {
    fn print(&self);
}

struct Foo;

impl Printable for Foo {
    fn print(&self) {
        // Simulate output
        unsafe {
            puts("called Foo::print()\0" as *const _ as *const i8);
        }
    }
}

fn get_printable() -> impl Printable {
    Foo
}

extern "C" {
    fn puts(s: *const i8);
}

fn main() -> i32 {
    let p = get_printable();
    p.print();

    0
}
