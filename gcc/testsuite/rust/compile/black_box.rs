// { dg-options "-fdump-tree-gimple" }
#![feature(rustc_attrs)]

#[lang = "sized"]
pub trait Sized {}

#[rustc_builtin_macro]
macro_rules! llvm_asm {
    () => {};
}

pub fn black_box<T>(mut dummy: T) -> T {
    unsafe {
        // { dg-final { scan-tree-dump-times {memory} 1 gimple } }
        llvm_asm!("" : : "r"(&mut dummy) : "memory" : "volatile");
    }

    dummy
}

fn my_function(a: i32) -> i32 {
    a
}

fn main() {
    let dummy: i32 = 42;
    let _ = black_box(my_function(dummy));
}
