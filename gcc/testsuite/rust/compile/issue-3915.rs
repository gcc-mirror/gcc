// { dg-options "-w" }
#[lang = "sized"]
trait Sized {}

trait Trait {
    fn do_thing();
}

struct MyType;

impl Trait for MyType {
    fn do_thing() {}
}

struct Wrapper<T: Trait> {
    value: T,
}

impl<T: Trait> Wrapper<T> {
    fn call_it() {
        T::do_thing();
    }
}

fn main() {
    let _ = Wrapper::<MyType> { value: MyType };
    Wrapper::<MyType>::call_it();
}
