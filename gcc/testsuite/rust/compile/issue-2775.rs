// { dg-options "-w" }
#[lang = "sized"]
pub trait Sized {}

struct Ref<'a, T> {
    x: &'a T,
}

pub fn test<'a, 'b, 'c>() {
    let (_, &&Ref::<(&'_ i32, i32)> { x: &(a, b) }): (i32, &'_ &'b Ref<'b, (&'c i32, i32)>);
}
