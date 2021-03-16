fn main() {
    let array: [i32; 3] = [0; 3];
    array[0] = 1; // { dg-error "cannot assign to immutable" }
}
