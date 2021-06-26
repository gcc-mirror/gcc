fn foo() -> bool {
    return true;
}

fn bar() -> bool {
    return false;
}



fn main() {
    let _a = true && foo();
    let _b = true || bar();
}