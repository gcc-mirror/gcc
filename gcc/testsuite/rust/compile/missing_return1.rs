// { dg-error "expected .* got .*" "" { target *-*-* } 0 }
fn test1() -> i32 {}

fn main() {
    let call1 = test1();
}
