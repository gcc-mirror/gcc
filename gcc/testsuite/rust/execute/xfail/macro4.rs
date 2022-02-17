// { dg-output "any\nany\nany\nany\nany\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

fn f() {
    let r_s = "any\n\0";
    let s_p = r_s as *const str;
    let c_p = s_p as *const i8;

    printf(c_p);
}

macro_rules! any {
    ($($a:expr)*) => {
        $($a;)*
    }
}

fn main() {
    any!(); // valid, but does not print anything
    any!(f() f());
    any!(f() f()    f());
}
