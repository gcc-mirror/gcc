extern "C" {
    fn printf(s: *const i8, ...);
}

#[rustc_builtin_macro] //{ dg-error "internal implementation detail. " "" { target *-*-* }  }
macro_rules! line {
    () => {{}};
}

fn main() -> i32 {
    let a = line!();
    printf("%d\0" as *const str as *const i8, a);

    0
}
