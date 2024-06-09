// { dg-additional-options "-fsyntax-only" }

fn main() {
    let is_zero = &|&&d: &&u8| -> bool { d == b'0' };
    let lambda = |&c| c != b'9';
}
