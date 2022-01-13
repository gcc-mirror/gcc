// { dg-additional-options "-w" }
const fn const_fn() -> usize {
    4
}

const FN_TEST: usize = const_fn();

const TEST: usize = 2 + FN_TEST;

fn main() -> i32 {
    let a: [_; 12] = [5; TEST * 2];
    a[6] - 5
}
