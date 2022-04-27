
fn main () {

    #[repr(align(8))]
    struct Baz {
        x: u16,
        y: u32,
    };

    #[repr(align(4))]
    struct Qux (u8, i16);

    let b = Baz { x: 5, y: 1984 };
    // { dg-warning "unused name" "" { target *-*-* } .-1 }

    let c = Qux (1, 2);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
