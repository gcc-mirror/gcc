#[lang = "sized"]
trait Sized {}

trait Foo {
    fn id(&self) -> i32;
}

struct Thing(i32);

impl Foo for Thing {
    fn id(&self) -> i32 {
        self.0
    }
}

fn make_thing(a: i32) -> impl Foo {
    Thing(a)
}

fn use_foo(f: impl Foo) -> i32 {
    f.id()
}

fn main() -> i32 {
    let value = make_thing(42);
    let val = use_foo(value);
    val - 42
}
