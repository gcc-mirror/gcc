fn main() {
    struct G {
        g: (),
    }
    let g = [0; G { g: () }];
    // { dg-error "mismatched types, expected .usize. but got .G. .E0308." "" { target *-*-* } .-1 }
}
