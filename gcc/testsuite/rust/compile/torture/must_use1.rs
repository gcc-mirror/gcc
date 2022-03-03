#[must_use = "TEST 1"]
fn test1() -> i32 {
    123
}

#[must_use = "TEST 2"]
fn test2() -> i32 {
    456
}

fn main() {
    let _a = test1();

    test2();
    // { dg-warning "ignoring return value of" "" { target *-*-* } .-1 }
}
