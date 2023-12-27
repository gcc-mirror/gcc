const A: &'static u8 = b"
";
const B: &'static str = b"
";
const C: &'static u8 = "
";
const D: &'static str = "
";
ERROR_TIME
// { dg-error "unrecognised token" "" { target *-*-* } .-1 }
// { dg-error "failed to parse item in crate" "" { target *-*-* } .-2 }
