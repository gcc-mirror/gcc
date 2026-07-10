#![feature(no_core, rustc_attrs)]
#![no_core]

#[rustc_conversion_suggestion] // { dg-warning "...rustc_conversion_suggestion.. is not implemented yet and has no effect" }
pub fn to_string() {}

#[rustc_conversion_suggestion] // { dg-warning "...rustc_conversion_suggestion.. is not implemented yet and has no effect" }
const TO_STRING : i32 = 0; // { dg-error "...rustc_conversion_suggestion.. can only be applied to functions" } 

#[rustc_conversion_suggestion] // { dg-warning "...rustc_conversion_suggestion.. is not implemented yet and has no effect" }
static TO_STRING2 : i32 = 0; // { dg-error "...rustc_conversion_suggestion.. can only be applied to functions" }

#[rustc_conversion_suggestion] // { dg-warning "...rustc_conversion_suggestion.. is not implemented yet and has no effect" }
struct TO_STRING3; // { dg-error "...rustc_conversion_suggestion.. can only be applied to functions" }
