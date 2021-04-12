fn test() -> bool {  // { dg-error "was defined here" }
    true
}

fn test() -> i32 { // { dg-error "redefined multiple times" }
    123
}

fn main() {}
