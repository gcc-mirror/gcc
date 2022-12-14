fn main() {
    let a = 123;
    let b = (a,);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
