// { dg-additional-options "-w" }
#[lang = "sized"]
pub trait Sized {}

#[lang = "index"]
trait Index<Idx> {
    type Output;

    fn index(&self, index: Idx) -> &Self::Output;
}

struct Foo(i32, i32);
impl Index<isize> for Foo {
    type Output = i32;

    fn index(&self, index: isize) -> &i32 {
        if index == 0 {
            &self.0
        } else {
            &self.1
        }
    }
}

fn main() -> i32 {
    let a = Foo(1, 2);
    let b = a[0];
    let c = a[1];

    c - b - 1
}
