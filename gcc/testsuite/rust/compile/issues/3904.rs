#![feature(no_core)]
#![no_core]

trait A {}
impl A for i32 {}
fn main() {
    let _a: &dyn A = &1;
}

static STATIC_1: dyn = 1; // {  dg-error "failed to parse TraitObjectType initial bound" "" { target *-*-* }   }
static STATIC_2: dyn = *#[serde()]; // {  dg-error "failed to parse TraitObjectType initial bound" "" { target *-*-* }   }
                                    // { dg-error "found unexpected token .;. in null denotation" "" { target *-*-* } .-1 }
