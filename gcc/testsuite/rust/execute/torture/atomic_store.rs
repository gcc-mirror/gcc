trait Copy {}

extern "rust-intrinsic" {
    pub fn atomic_store_seqcst<T: Copy>(dst: *mut T, val: T);
    pub fn atomic_store_release<T: Copy>(dst: *mut T, val: T);
    pub fn atomic_store_relaxed<T: Copy>(dst: *mut T, val: T);
    pub fn atomic_store_unordered<T: Copy>(dst: *mut T, val: T);
}

fn main() -> u32 {
    let mut dst = 15u32;
    let one;
    let two;
    let three;
    let four;

    unsafe {
        atomic_store_seqcst(&mut dst, 1);
        one = dst;

        atomic_store_release(&mut dst, 2);
        two = dst;

        atomic_store_relaxed(&mut dst, 3);
        three = dst;

        atomic_store_unordered(&mut dst, 4);
        four = dst;
    }

    (four + three + two + one) - 10
}
