// { dg-options "-w -O0 -fdump-tree-gimple" }
const fn test(mut x: i32) -> i32 {
    loop {
        if x == 10 {
            break;
        }

        x = x + 1;
    }
    return x;
}

const X: i32 = test(0);

fn main() {
    // { dg-final { scan-tree-dump-times {x = 10} 1 gimple } }
    let x = X;
}
