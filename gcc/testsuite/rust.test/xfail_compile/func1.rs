fn test(x: i32) -> bool { // { dg-error "expected .bool. got .<tyty::error>.." }
    return x + 1; // { dg-error "expected .bool. got .i32." }
}

fn main() {
    let an_integer = 5;

    let call_test = test(1);
}
