// { dg-options "-w -O0 -fdump-tree-gimple" }
const A: i32 = 1;

const fn test(a: i32) -> i32 {
    let b = A + a;
    if b == 2 {
        return b + 2;
    }
    a
}

const B: i32 = test(1);
const C: i32 = test(12);

fn main() {
    // { dg-final { scan-tree-dump-times {a = 1} 1 gimple } }
    let a = A;
    // { dg-final { scan-tree-dump-times {b = 4} 1 gimple } }
    let b = B;
    // { dg-final { scan-tree-dump-times {c = 12} 1 gimple } }
    let c = C;
}
