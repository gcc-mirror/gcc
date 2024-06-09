// { dg-options "-w" }
#[lang = "sized"]
pub trait Sized {}

#[lang = "deref"]
trait Deref {
    type Target;
    fn deref(&self) -> &Self::Target;
}

fn foo<T: Deref<Target = i32>>(t: &T) -> i32 {
    t.max(2)
}

impl i32 {
    fn max(self, other: i32) -> i32 {
        if self > other {
            self
        } else {
            other
        }
    }
}
