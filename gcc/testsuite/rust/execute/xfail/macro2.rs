// { dg-output "arg\narg\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

fn f() {
    let r_s = "arg\n\0";
    let s_p = r_s as *const str;
    let c_p = s_p as *const i8;

    printf(c_p);
}

macro_rules! kw0 {
    (keyword) => { f() };
}

macro_rules! kw1 {
    (fn) => { f() };
}

macro_rules! kw2 {
    (kw0 kw1 kw3) => { f() };
}

fn main() {
    kw0!(keyword);
    kw1!(fn);
    kw2!(kw0 kw1 kw3);
}
