fn test1() -> i32 {
    return 2;
    // { dg-warning "unreachable expression" "" { target *-*-* } .+1 }
    1
}

fn test2(x: i32) -> i32 {
    if x > 1 {
        return 5;
    } else {
        return 0;
    }
    // { dg-warning "unreachable statement" "" { target *-*-* } .+1 }
    return 1;
}

fn main() {
    let call1 = test1();
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
    let call2 = test2(2);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
