#[lang = "sized"]
trait Sized {}

pub struct S<T>(T);

pub fn foo<T>(x: T) {
    let y = S(x);
    y.0;
}
