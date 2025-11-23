#![feature(lang_items)]
#[lang = "sized"]
trait Sized {}

#[allow(unused)]
macro_rules! simd_shuffle {
    ($x:expr, $y:expr, $idx:expr $(,)?) => {{
        simd_shuffle(
            $x,
            $y,
            const {
                let v: [u32; _] = $idx;
                v
            },
        )
    }};
}

const fn simd_shuffle(_a: [u32; 4], _b: [u32; 4], idx: [u32; 4]) -> [u32; 4] {
    idx
}

fn main() -> i32 {
    let a = [1, 2, 3, 4];
    let b = [5, 6, 7, 8];
    let indices = [3, 2, 1, 0];

    let result: [u32; 4] = simd_shuffle!(a, b, indices);

    if result[0] != 3 {
        return 1;
    }
    if result[1] != 2 {
        return 2;
    }
    if result[2] != 1 {
        return 3;
    }
    if result[3] != 0 {
        return 4;
    }

    0
}
