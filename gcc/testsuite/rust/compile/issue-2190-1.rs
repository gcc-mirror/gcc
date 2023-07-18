#[lang = "sized"]
pub trait Sized {}

#[lang = "deref"]
trait Deref {
    type Target;
    fn deref(&self) -> &Self::Target;
}

fn foo<T: Deref<Target = i32>>(t: &T) -> i32 {
    t.max(2)
    // { dg-error "failed to resolve method for .max." "" { target *-*-* } .-1 }
}
