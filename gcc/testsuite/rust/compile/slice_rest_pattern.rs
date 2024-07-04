// { dg-options "-fsyntax-only" }
fn foo(a: &[u32]) {
    match a {
        [first, ..] => {}
        [.., last] => {}
        _ => {}
    }
}
