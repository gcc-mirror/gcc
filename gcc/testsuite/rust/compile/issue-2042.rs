fn f<'r>(p: &'r mut fn(p: &mut ())) {
    (*p)(())
    // { dg-error "expected .&mut ()." "" { target *-*-* } .-1 }
}

fn main() {}
