// { dg-options "-w" }
#![feature(lang_items)]
#[lang = "sized"]
trait Sized {}

struct Matrix<T, const ROWS: usize, const COLS: usize> {
    data: [[T; COLS]; ROWS],
}

fn main() {
    let _: Matrix<u8, 2, 3> = Matrix {
        data: [[1, 2, 3], [4, 5, 6]],
    };
}
