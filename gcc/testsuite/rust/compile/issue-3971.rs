#[lang = "copy"]
trait Copy {}

// since the macro expansion fails, the current nameres fixpoint error is emitted - just accept it for now
#[derive(Copy)]
// { dg-error "derive may only be applied to structs, enums and unions" "" { target *-*-* } .-1 }
// { dg-excess-errors "could not resolve trait" }

pub fn check_ge(a: i32, b: i32) -> bool {
    a >= b
}
