#[lang = "sized"]
trait Sized {}

pub trait T<X> {
    const D: i32 = {
        // { dg-error "mismatched types, expected .i32. but got .()." "" { target *-*-* } .-1 }
        const C: X;
    };
}
