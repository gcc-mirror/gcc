fn main() {
    let a: () = ();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let b;
    b = ();
}
