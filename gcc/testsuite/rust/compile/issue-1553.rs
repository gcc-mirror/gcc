// { dg-options "-w -O0 -fdump-tree-gimple" }
#![feature(no_core)]
#![no_core]

const fn test(x: i32) -> i32 {
    return match x {
        0 => 100,
        _ => 200,
    };
}

const X: i32 = test(0);
const Y: i32 = test(1);

fn main() {
    // { dg-final { scan-tree-dump-times {x = 100} 1 gimple } }
    let x = X;
    // { dg-final { scan-tree-dump-times {y = 200} 1 gimple } }
    let y = Y;
}
