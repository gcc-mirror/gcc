#[lang = "sized"]
pub trait Sized {}

trait A {
    fn a() -> i32 {
        123
    }

    fn b() -> i32 {
        Self::a() + 456
    }
}

struct S;
impl A for S {}

fn main() {
    let a;
    a = S::a();

    let b;
    b = S::b();
}
