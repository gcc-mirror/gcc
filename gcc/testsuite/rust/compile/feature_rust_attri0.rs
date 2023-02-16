#[rustc_builtin_macro] //{ dg-error "internal implementation detail. " "" { target *-*-* }  }
macro_rules! line {
    () => {{}};
}

fn main() -> i32 {
    let a = line!();
    print(a);

    0
}
