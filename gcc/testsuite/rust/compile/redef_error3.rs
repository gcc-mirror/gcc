fn test() -> bool {
    true
}

fn test() -> i32 { // { dg-error "defined multiple times" }
    123
}

fn main() {}
