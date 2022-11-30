trait Copy {}

extern "rust-intrinsic" {
    pub fn atomic_load_seqcst<T: Copy>(src: *const T) -> T;
    pub fn atomic_load_acquire<T: Copy>(src: *const T) -> T;
    pub fn atomic_load_relaxed<T: Copy>(src: *const T) -> T;
    pub fn atomic_load_unordered<T: Copy>(src: *const T) -> T;
}

fn main() -> u32 {
    let one;
    let two;
    let three;
    let four;

    unsafe {
        let mut src = 1u32;
        one = atomic_load_seqcst(&src);

        src = 2;
        two = atomic_load_acquire(&src);

        src = 3;
        three = atomic_load_relaxed(&src);

        src = 4;
        four = atomic_load_unordered(&src);
    }

    (four + three + two + one) - 10
}
