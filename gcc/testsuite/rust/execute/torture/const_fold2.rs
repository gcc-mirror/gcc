// { dg-additional-options "-w" }
const A: i32 = 1;
const B: i32 = { A + 2 };

const fn test() -> i32 {
    B
}

const C: i32 = {
    const a: i32 = 4;
    test() + a
};

fn main() -> i32 {
    C - 7
}
