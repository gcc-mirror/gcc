#[lang = "sized"]
trait Sized {}

pub trait Foo {}

pub trait Bar {
    type Baz;
}

struct MyBaz; // { dg-warning "struct is never constructed" }
impl Foo for MyBaz {}

struct MyBar;

impl Bar for MyBar {
    type Baz = MyBaz;
}

pub fn foo<T, U>(_value: T) -> i32
where
    T: Bar<Baz = U>,
    U: Foo,
{
    15
}

fn main() -> i32 {
    let bar = MyBar;
    let result: i32 = foo::<MyBar, MyBaz>(bar);

    result - 15
}
