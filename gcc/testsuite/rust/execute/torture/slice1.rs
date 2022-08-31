// { dg-additional-options "-w" }
struct FatPtr<T> {
    data: *const T,
    len: usize,
}

union Repr<T> {
    rust: *const [T],
    rust_mut: *mut [T],
    raw: FatPtr<T>,
}

const fn slice_from_raw_parts<T>(data: *const T, len: usize) -> *const [T] {
    unsafe {
        Repr {
            raw: FatPtr { data, len },
        }
        .rust
    }
}

fn main() -> i32 {
    let a = 123;
    let b: *const i32 = &a;
    let c = slice_from_raw_parts(b, 1);

    0
}
