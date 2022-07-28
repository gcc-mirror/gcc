mod mem {
    extern "rust-intrinsic" {
        fn size_of<T>() -> usize;
        fn transmute<U, V>(_: U) -> V; // { dg-error "cannot transmute between types of different sizes, or dependently-sized types" }
    }
}

fn main() {
    let a = 123;
    let _b: [u32; mem::size_of::<i32>()] = unsafe { mem::transmute(a) };
}
