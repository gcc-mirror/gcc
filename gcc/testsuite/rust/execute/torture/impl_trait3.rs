/* { dg-output "Hello from Message\r*\n" } */
#[lang = "sized"]
pub trait Sized {}

extern "C" {
    fn printf(s: *const i8, ...);
}

trait Speak {
    fn speak(&self) -> &'static str;
}

trait Printer {
    fn print(&self, input: impl Speak);
}

struct Console;

impl Printer for Console {
    fn print(&self, input: impl Speak) {
        // { dg-warning "unused name .self." "" { target *-*-* } .-1 }
        unsafe {
            let a = input.speak();
            let b = a as *const str;
            let c = b as *const i8;

            printf(c);
        }
    }
}

struct Message(&'static str);

impl Speak for Message {
    fn speak(&self) -> &'static str {
        self.0
    }
}

fn main() -> i32 {
    let c = Console;
    let msg = Message("Hello from Message\n");
    c.print(msg);

    0
}
