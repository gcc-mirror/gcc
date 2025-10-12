enum E {
    V(usize),
}

struct S {
    field: i32,
}

fn main() {
    let e = E::V(0);
    let _ = e.0; // { dg-error "expected tuple or tuple struct, found 'E'" }

    let s = S { field: 0 };
    let _ = s.0; // { dg-error "expected tuple or tuple struct, found 'S'" }
}
