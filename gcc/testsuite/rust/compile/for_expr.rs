// { dg-options "-fsyntax-only" }

fn main() {
    for a in 0..10 {}
    (for b in 0..10 {})
}
