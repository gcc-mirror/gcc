fn size() -> usize {
    15
}

fn main() {
    let a = [15; size()]; // { dg-error "calls in constants are limited to constant functions, tuple structs and tuple variants" }
}
