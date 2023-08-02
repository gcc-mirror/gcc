fn main() {
    let xs: [i32; 5] = [1, 2, 3, 4, 5];
    let a: bool = xs[0]; // { dg-error "mismatched types, expected .bool. but got .i32." }
}
