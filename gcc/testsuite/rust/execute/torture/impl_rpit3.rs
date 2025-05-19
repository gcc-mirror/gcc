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

fn make_thing() -> impl Foo {
    Thing(99)
}

fn main() -> i32 {
    let v = make_thing();
    let r = &v;
    let val = r.id();
    val - 99
}
