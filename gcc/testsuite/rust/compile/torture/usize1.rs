fn main() {
    let a = [1, 2, 3];
    let b: usize = 1;
    let c = a[b];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
