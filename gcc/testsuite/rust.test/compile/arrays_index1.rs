fn main() {
    let mut array: [i32; 3] = [0; 3];

    let a = array[0];
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let x = 0;
    let mut c;
    c = array[x+1];
}
