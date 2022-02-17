// { dg-output "oom\noom\noom\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

fn f() {
    let r_s = "oom\n\0";
    let s_p = r_s as *const str;
    let c_p = s_p as *const i8;

    unsafe { printf(c_p); }
}

macro_rules! one_or_more {
    ($($a:expr)+) => {
        $($a;)+
    }
}

fn main() {
    one_or_more!(f());
    one_or_more!(f() f());
}
