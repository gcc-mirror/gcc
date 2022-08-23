fn main() {
    let xs: [i32; 5] = [1, 2, 3, 4, 5];
    let xy = [6, 7, 8];

    let a = xs[0];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let b = xy[2];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
