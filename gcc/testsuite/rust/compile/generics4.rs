#[lang = "sized"]
pub trait Sized {}

struct GenericStruct<T>(T, usize);

fn main() {
    let a2;
    a2 = GenericStruct::<i8, i32>(1, 456); // { dg-error "generic item takes at most 1 type arguments but 2 were supplied" }

    let b2: i32 = a2.0;
    // { dg-error {Expected Tuple or ADT got: T\?} "" { target *-*-* } .-1 }
    let c2: usize = a2.1;
    // { dg-error {Expected Tuple or ADT got: T\?} "" { target *-*-* } .-1 }
}
