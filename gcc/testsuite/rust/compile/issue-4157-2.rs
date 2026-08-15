#![feature(no_core)]
#![no_core]
#![feature(lang_items)]

#[lang = "sized"]
trait Sized {}

struct MyStruct {
    field: usize,
}
struct Nested {
    nested: MyStruct,
}
struct Mix2 {
    nested: (Nested,),
}

const STRUCT: MyStruct = MyStruct { field: 42 };
const TUP: (usize,) = (43,);
const NESTED_S: Nested = Nested {
    nested: MyStruct { field: 5 },
};
const NESTED_T: ((usize,),) = ((4,),);
const MIX_1: ((Nested,),) = ((MyStruct { field: 3 },),);
// { dg-error "mismatched types, expected .Nested. but got .MyStruct." "" { target *-*-* } .-1 }
const MIX_2: Mix2 = Mix2 { nested: ((2,),) };
// { dg-error "mismatched types, expected .Nested. but got" "" { target *-*-* } .-1 }
const INSTANT_1: usize = (MyStruct { field: 1 }).field;
const INSTANT_2: usize = (4,).0;

fn main() {
    let h = [0; STRUCT.field];
    let b = [0; TUP.0];
    let c = [0; NESTED_S.nested.field];
    let d = [0; (NESTED_T.0).0];
    let e = [0; (MIX_1.0).0.nested.field];
    // { dg-error "failed to resolve TupleIndexExpr receiver" "" { target *-*-* } .-1 }
    // { dg-error "expected algebraic data type got .<tyty::error>." "" { target *-*-* } .-2 }
    let f = [0; (MIX_2.nested.0).0];
    // { dg-error "expected tuple or tuple struct, found .Nested." "" { target *-*-* } .-1 }
    let g = [0; INSTANT_1];
    let h = [0; INSTANT_2];
}
