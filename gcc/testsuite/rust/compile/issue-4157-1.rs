#![feature(no_core)]
#![no_core]
#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

struct MyStruct {}
struct Nested {
    nested: MyStruct,
}
struct Mix2 {
    nested: (Nested,),
}

const MIX_2: Mix2 = Mix2 { nested: ((2,),) };
// { dg-error "mismatched types, expected .Nested. but got" "" { target *-*-* } .-1 }

fn main() {
    let f = [0; (MIX_2.nested.0).0];
    // { dg-error "expected tuple or tuple struct, found .Nested." "" { target *-*-* } .-1 }
}
