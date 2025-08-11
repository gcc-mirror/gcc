pub fn foo(a: &[u32]) {
    match a {
        [first, ..] => {}
        [.., last] => {}
        _ => {}
    }
}
