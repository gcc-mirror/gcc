// { dg-error "expected .* got .*" "" { target { *-*-* } } 0 }

fn test(x: i32) -> i32 {
    if x > 1 {
        1
    } else {
        2
    }
    3
}

fn main() {
    let a = test(1);
}
