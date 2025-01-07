enum Res {
    OK,
    BAD,
}

enum LOption {
    Some(i32),
    None,
}

fn test(v: LOption) -> Res {
    return Res::BAD;
}

fn main() {
    // Should be:
    // test(LOption::Some(2));
    //
    test(LOption(2));
    // { dg-error "expected function, tuple struct or tuple variant, found enum" "" { target *-*-* } .-1 }
}
