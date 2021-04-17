fn test() -> bool {
    true
}

fn test() -> i32 { // { dg-error "redefined multiple times" }
    123
}

fn main() {}
