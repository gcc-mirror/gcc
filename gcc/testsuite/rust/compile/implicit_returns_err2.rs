fn test(x: i32) -> i32 {
    // { dg-error "mismatched types, expected .i32. but got .bool." "" { target *-*-* } .-1 }
    return 1;
    // { dg-warning "unreachable expression" "" { target *-*-* } .+1 }
    true
}

fn main() {
    let a = test(1);
}
