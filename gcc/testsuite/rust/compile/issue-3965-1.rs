fn main() {
    [(); { continue }];
    // { dg-error ".continue. outside of a loop .E0268." "" { target *-*-* } .-1 }
}
