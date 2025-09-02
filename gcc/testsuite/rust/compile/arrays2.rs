fn main() {
    let array: [i32; 5] = [1, 2, 3];
    // { dg-error "mismatched types, expected ..i32; 5.. but got ...integer.; 3.. .E0308." "" { target *-*-* } .-1 }
}
