fn main() {
    let a = [1, 2, 3];
    let b: u32 = 1;
    let c = a[b]; // { dg-error "the type ...integer.; 3.. cannot be indexed by .u32." }
}
