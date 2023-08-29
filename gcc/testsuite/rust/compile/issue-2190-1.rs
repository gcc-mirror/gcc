#[lang = "sized"]
pub trait Sized {}

#[lang = "deref"]
trait Deref {
    type Target;
    fn deref(&self) -> &Self::Target;
}

fn foo<T: Deref<Target = i32>>(t: &T) -> i32 {
    t.max(2)
    // { dg-error "no method named .max. found in the current scope" "" { target *-*-* } .-1 }
}
