pub fn toto(a: i32, ...) {}
// { dg-error "only foreign or .unsafe extern .C.. functions may be C-variadic" "" { target *-*-* } .-1 }

fn main() {}
