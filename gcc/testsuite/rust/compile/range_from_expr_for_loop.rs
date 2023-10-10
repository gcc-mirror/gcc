// { dg-additional-options "-frust-compile-until=ast" }
fn main() {
    for _ in 1.. {
        break;
    }
    let i = 2;
}
