struct S(
    [u8; {
        {
            // { dg-error "mismatched types. expected .... but got ..integer.. .E0308." "" { target *-*-* } .-1 }
            struct Z;
            0
        }
        0
    }],
);

fn main() {}
