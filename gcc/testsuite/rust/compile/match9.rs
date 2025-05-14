enum E {
    A(),
    B,
}

const CONST_E: E = E::A();

static static_e: E = E::A();

type type_alias = E;

fn f(e: E) {
    match e {
        E::A => {}
        // { dg-error "expected unit struct, unit variant or constant, found tuple variant .E::A." "" { target *-*-* } .-1 }
        E::B => {}
        crate::CONST_E => {}
        crate::type_alias => {}
        // { dg-error "expected unit struct, unit variant or constant, found type alias .crate::type_alias." "" { target *-*-* } .-1 }
        crate::E => {}
        // { dg-error "expected unit struct, unit variant or constant, found enum .crate::E." "" { target *-*-* } .-1 }
        crate::static_e => {}
        // { dg-error "expected unit struct, unit variant or constant, found static .crate::static_e." "" { target *-*-* } .-1 }
        crate::f => {}
        // { dg-error "expected unit struct, unit variant or constant, found function .crate::f." "" { target *-*-* } .-1 }
        _ => {}
    }
}

fn main() {}
