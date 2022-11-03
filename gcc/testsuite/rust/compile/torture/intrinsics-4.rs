trait Copy {}

extern "rust-intrinsic" {
    pub fn atomic_store_seqcst<T: Copy>(dst: *mut T, val: T);
    pub fn atomic_store_release<T: Copy>(dst: *mut T, val: T);
    pub fn atomic_store_relaxed<T: Copy>(dst: *mut T, val: T);
    // pub fn atomic_store_unordered<T: Copy>(dst: *mut T, val: T);
}

fn main() {
    let mut dst = 15u32;
    let new_value = 14;

    unsafe {
        atomic_store_seqcst(&mut dst, new_value);
        atomic_store_release(&mut dst, new_value);
        atomic_store_relaxed(&mut dst, new_value);
        // atomic_store_unordered(&mut dst, new_value);
    }
}
