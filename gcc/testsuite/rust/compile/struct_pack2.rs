
fn main () {

    #[repr(packed(2))]
    struct Baz {
        x: u16,
        y: u32,
    };

    #[repr(packed)]
    struct Qux (u8, i16);

    let b = Baz { x: 5, y: 1984 };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let c = Qux (1, 2);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
