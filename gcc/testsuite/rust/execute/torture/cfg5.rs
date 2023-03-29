// { dg-additional-options "-w -frust-cfg=A" }

fn main() -> i32 {
    let mut a = 0;

    #[cfg(A)]
    a = 3;

    #[cfg(B)]
    a = 40;

    a - 3
}
