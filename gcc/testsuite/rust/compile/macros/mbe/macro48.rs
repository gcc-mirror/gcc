// Check that "priv" is not in the follow set of :vis.

// { dg-error "token .priv. is not allowed after .vis. fragment" "#359" { target *-*-* } .+4 }
// { dg-error "required first macro rule in macro rules definition could not be parsed" "" { target *-*-* } .+3 }
// { dg-error "failed to parse item in crate" "" { target *-*-* } .+2 }
macro_rules! my_mac {
    ($v:vis priv) => {
        $v struct Foo(i32);
    }
}
