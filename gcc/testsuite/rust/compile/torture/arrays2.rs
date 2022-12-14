fn main() {
    let mut array: [i32; 3] = [0; 3];

    let a = array[0];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let mut c;
    c = array[2];
}
