macro_rules! maybe_return { ($e:expr) => ($e); }

fn frob(x: i32) -> i32{
    maybe_return! {x}
    // { dg-error "mismatched types. expected .... but got .i32. .E0308." "" { target *-*-* } .-1 }
    // should return -1 
    -1
}
