fn main() {
    let a = [1, 2, 3];
    let b: u32 = 1;
    let c = a[b]; // { dg-error "the type ...integer..CAPACITY.. cannot be indexed by .u32." }
                  // { dg-error {failed to type resolve expression} "" { target *-*-* } .-1 }
}
