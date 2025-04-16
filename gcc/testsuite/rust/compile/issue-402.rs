#[lang = "sized"]
pub trait Sized {}

struct GenericStruct<T>(T, usize);

pub fn test() -> GenericStruct<_> {
    // { dg-error "the type placeholder ._. is not allowed within types on item signatures .E0121." "" { target *-*-* } .-1 }
    GenericStruct(1, 2)
}

fn square(num: i32) -> _ {
    // { dg-error "the type placeholder ._. is not allowed within types on item signatures .E0121." "" { target *-*-* } .-1 }
    num * num
}
