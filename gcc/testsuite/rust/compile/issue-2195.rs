#[lang = "sized"]
pub trait Sized {}

struct A<T> {
    // { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
    f: *const T,
}

pub fn cast<T>(a: A<T>) {
    let z = a.f as *const ();
}
