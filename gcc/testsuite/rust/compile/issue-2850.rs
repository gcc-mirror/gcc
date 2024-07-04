fn myfun0(...,_:i32) {}
// { dg-error "only foreign or .unsafe extern \"C\". functions may be C-variadic" "" { target *-*-* } .-1 }

fn myfun1(a:i32,...,_:i32) {}
// { dg-error "only foreign or .unsafe extern \"C\". functions may be C-variadic" "" { target *-*-* } .-1 }

struct z {
    x: f64,
    y: f64,
}

impl z {
    fn new(x: f64, ..., y: f64) -> z {
        // { dg-error "only foreign or .unsafe extern \"C\". functions may be C-variadic" "" { target *-*-* } .-1 }
        z { x: x, y: y }
    }
}