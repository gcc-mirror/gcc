struct S;

fn main() {
    let s = S;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
