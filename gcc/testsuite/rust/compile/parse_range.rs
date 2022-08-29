// { dg-additional-options "-fsyntax-only" }

fn main() {
    let a = [1, 2, 3, 4];
    let _ = a[0..];
    let _ = a[..3];
    let _ = a[0..3];
    let _ = a[..];
}
