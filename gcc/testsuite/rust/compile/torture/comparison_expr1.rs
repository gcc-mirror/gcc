fn is_zero(x: i32) -> bool {
    x == 0
}

fn is_not_zero(x: i32) -> bool {
    x != 0
}

fn is_positive(x: i32) -> bool {
    x > 0
}

fn is_negative(x: i32) -> bool {
    x < 0
}

fn is_positive_or_zero(x: i32) -> bool {
    x >= 0
}

fn is_negative_or_zero(x: i32) -> bool {
    x <= 0
}

fn main() {
    let a: bool = is_zero(1);
    let b: bool = is_not_zero(2);
    let c: bool = is_positive(3);
    let d: bool = is_negative(4);
    let e: bool = is_positive_or_zero(5);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let f: bool = is_negative_or_zero(6);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let g: bool = a || b;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let h: bool = c && d;
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
