// { dg-options "-w" }
// Test for issue #4233 - malformed #[target_feature] attribute input

#[target_feature] // { dg-error "malformed .target_feature. attribute input" }
unsafe fn foo_sse() {} 
// { dg-note "must be of the form" "" { target *-*-* } .-2 }
