//! test_mod inner doc comment
//!
//! foo bar baz cake pizza carbs

pub struct Test(pub i32);
// { dg-warning "struct is never constructed" "" { target *-*-* } .-1 }
