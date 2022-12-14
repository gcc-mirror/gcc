fn main() {
    let y = x + 1;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}

static x: i32 = 3;
