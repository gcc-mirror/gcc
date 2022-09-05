// { dg-options "-w -O0 -fdump-tree-gimple" }
struct Foo(usize, usize);

const A:Foo = Foo(123, 4546);

const B:usize = A.0;

fn main() {
    // { dg-final { scan-tree-dump-times {b = 123} 1 gimple } }
    let b = B;
}

