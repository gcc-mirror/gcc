#![feature(extern_types, lang_items, no_core)]
#![no_core]

#[lang = "sized"]
trait Sized {}

extern "C" {
    type Opaque;
}

fn require_sized<T>() {}

fn main() {
    require_sized::<Opaque>();
    // { dg-error "bounds not satisfied for Opaque .Sized. is not satisfied" "" { target *-*-* } .-1 }
}
