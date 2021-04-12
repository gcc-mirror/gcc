fn main() {
    let x = 111;

    let a = {
        // { dg-warning "unused name" "" { target *-*-* } .-1 }
        if x == 10 {
            123
        } else if x < 10 {
            456
        } else {
            789
        }
    };
}
