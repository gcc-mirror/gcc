// { dg-prune-output "warning: unused name" } as there are many of these expected.

fn main() {
    let a1: i8 = 1i8;
    let a2: i16 = 2i16;
    let a3: i32 = 3i32;
    let a4: i64 = 4i64;
    let a5: i128 = 5i128;

    let b1 = 1i8;
    let b2 = 2i16;
    let b3 = 3i32;
    let b4 = 4i64;
    let b5 = 5i128;

    let c1: u8 = 1u8;
    let c2: u16 = 2u16;
    let c3: u32 = 3u32;
    let c4: u64 = 4u64;
    let c5: u128 = 5u128;

    let d1 = 1u8;
    let d2 = 2u16;
    let d3 = 3u32;
    let d4 = 4u64;
    let d5 = 5u128;
}
