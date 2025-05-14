// { dg-additional-options "-fdump-tree-gimple" }
#![feature(rustc_attrs)]

#[rustc_builtin_macro]
macro_rules! concat {
    () => {};
}

macro_rules! a {
    () => {
        "hey"
    };
    ($($t:tt)*) => {
        "ho"
    };
}

extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn print_ptr(s: &str) {
    unsafe {
        printf("%p\n\0" as *const str as *const i8, s as *const str);
    }
}

fn print_str(s: &str) {
    unsafe {
        printf(
            "%s\n\0" as *const str as *const i8,
            s as *const str as *const i8,
        );
    }
}

// { dg-final { scan-assembler {"abheyho"} } }
static S: &str = concat!("a", 'b', a!(), a!(b c d e f a!()), '\0');

fn main() {
    print_ptr(S);
    print_str(S);
}
