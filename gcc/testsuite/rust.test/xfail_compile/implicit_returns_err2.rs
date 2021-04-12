// { dg-error "expected .* got .*" "" { target { *-*-* } } 0 }

fn test(x: i32) -> i32 {
    return 1;
    // { dg-warning "unreachable expression" "" { target *-*-* } .+1 }
    true
}

fn main() {
    let a = test(1);
}
