trait Copy {}

extern "rust-intrinsic" {
    pub fn atomic_store_seqcst<T: Copy>(dst: *mut T, value: T);
    // { dg-error "atomic intrinsics can only be used with basic integer types .got .VeryLargeType.." "" { target *-*-* } .-1 }
    // { dg-error "atomic intrinsics can only be used with basic integer types .got .bool.." "" { target *-*-* } .-2 }
}

struct VeryLargeType {
    a0: i128,
    a1: i128,
    a2: i128,
    a3: i128,
}

impl VeryLargeType {
    pub fn new(value: i128) -> VeryLargeType {
        VeryLargeType {
            a0: value,
            a1: 0,
            a2: 0,
            a3: 0,
        }
    }
}

fn main() {
    let mut dst = VeryLargeType::new(0);
    let mut b = false;

    unsafe {
        atomic_store_seqcst(&mut dst, VeryLargeType::new(1));
        atomic_store_seqcst(&mut b, true);
    }
}
