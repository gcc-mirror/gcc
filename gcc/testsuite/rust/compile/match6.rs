fn foo() -> bool {
    true
}

fn int32() -> i32 {
    1
}

fn bar() -> i32 {
    match foo() {
        true => int32(),
        false => 0
    }
}

fn main() -> () {
    bar();
}