// { dg-do compile }
// { dg-options "-O1 -gdwarf-5 -dA -w" }
macro_rules! stmt {
    ($s:stmt) => {
        $s
    };
    ($s:stmt, $($ss:stmt),*) => {
        $s;
        stmt!($($ss),*);
    };
}

pub fn test() -> i32 {
    stmt!(
        let a = 1
    );
    stmt!(
        let b = 2,
        let c = 3,
        let d = 4,
        let e = 5,
        let f = b + c + d + e
    );
    // { dg-final { scan-assembler "14" } }
    f
}

fn main() {
    let _ = test();
}

