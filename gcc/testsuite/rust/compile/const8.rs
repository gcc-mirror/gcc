// { dg-options "-w -O0 -fdump-tree-gimple" }
struct Foo {
    First: i32,
    Second: f32
}

struct Bar {
    First: i32,
    Second: f32
}

struct Baz {
    First: i32,
    Second: f32
}
const A:Foo = Foo { First: 1, Second: 1.0 };
const B:Bar = Bar { First: 2, Second: 2.0 };
const C:Baz = Baz { First: 3, Second: 2.0 };

const fn test() -> f32 {
    if (A.First == 2) {
        return A.Second;
    }
    else if (B.First == 2) {
        return B.Second;
    }
    else if (C.First == 2) {
        return C.Second;
    }

    return 0.0;
}

const D:f32 = test();

fn main() {
    // { dg-final { scan-tree-dump-times {d = 2.0} 1 gimple } }
    let d = D;
}

