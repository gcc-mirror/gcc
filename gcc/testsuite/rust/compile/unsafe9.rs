union U {
    a: i32,
    b: f32,
    c: u8,
}

fn main() {
    let u = U { a: 14 };
    let _ = u.a; // { dg-error "access to union" }
}
