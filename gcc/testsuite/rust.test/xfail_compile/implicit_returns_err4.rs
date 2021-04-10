fn test(x: bool) -> bool {
    // { dg-error "expected .bool. got ...." "" { target *-*-*} .-1 }
    return x;
    // { dg-warning "unreachable expression" "" { target *-*-* } .+1 }
    ()
}

fn main() {
    let a = test(true);
}
