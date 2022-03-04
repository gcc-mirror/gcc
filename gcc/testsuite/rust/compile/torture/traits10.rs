trait Foo // where
//     Self: Sized,
{
    fn get(self) -> i32;

    fn test(self) -> i32 {
        self.get()
    }
}

struct Bar(i32);
impl Foo for Bar {
    fn get(self) -> i32 {
        self.0
    }
}

fn main() {
    let a;
    a = Bar(123);

    let b;
    b = Bar::get(a);

    let a;
    a = Bar(123);

    let b;
    b = a.test();
}
