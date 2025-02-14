fn func(i: i32) {
    i();
    // { dg-error "expected function, found .i32. .E0618." "" { target *-*-* } .-1 }
}

fn main() {
    let i = 0i32;
    i();
    // { dg-error "expected function, found .i32. .E0618." "" { target *-*-* } .-1 }
}
