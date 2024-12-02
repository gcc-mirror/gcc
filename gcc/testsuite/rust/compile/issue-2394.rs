const A: i32 = (1 / 0);
// { dg-error "division by zero" "" { target *-*-* } .-1 }

fn main() {
    let a = 1 / 0;
    // { dg-error "division by zero" "" { target *-*-* } .-1 }

    let b = 3;
    let c = b / 0;
    // { dg-error "division by zero" "" { target *-*-* } .-1 }

    let a = 1 << 500;
    // { dg-error "left shift count >= width of type" "" { target *-*-* } .-1 }
}
