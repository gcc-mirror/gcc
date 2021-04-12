fn gcd(x: i32, y: i32) -> i32 {
    if y == 0 {
        x
    } else {
        gcd(y, x % y)
    }
}

fn main() {
    let a = gcd(100, 5);
    // { dg-warning "unused name" "" { target *-*-* } .-1 }
}
