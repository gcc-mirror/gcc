// { dg-error "mismatched types, expected .i32. but got .i8." "" { target *-*-* } 0 }
#[lang = "sized"]
pub trait Sized {}

struct GenericStruct<T>(T, usize);

fn main() {
    let a2;
    a2 = GenericStruct::<i8>(1, 456);

    let b2: i32 = a2.0;
    let c2: usize = a2.1;
}
