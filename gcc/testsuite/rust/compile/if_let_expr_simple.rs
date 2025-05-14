enum MyOption {
    Some(i32),
    None,
}

pub fn toto(i : MyOption) -> i32 {
    if let MyOption::Some(v) = i {
        v
    } else {
        23i32
    }
}
