// { dg-options "-w" }
#[lang = "sized"]
trait Sized {}

trait Printable {
    fn print(&self);
}

struct Foo;

impl Printable for Foo {
    fn print(&self) {}
}

fn take_printable(_: impl Printable) {}
