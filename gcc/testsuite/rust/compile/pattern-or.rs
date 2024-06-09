// { dg-additional-options "-fsyntax-only" }

fn main() {
    match ((12, 13)) {
        (_, 5) | (12, _) => {}
    }
}
