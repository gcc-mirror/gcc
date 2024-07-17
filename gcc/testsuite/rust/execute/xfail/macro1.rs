// { dg-output "macro\r*\nmacro\r*\nmacro\r*\nmacro\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

fn f() {
    let r_s = "macro\n\0";
    let s_p = r_s as *const str;
    let c_p = s_p as *const i8;

    printf(c_p);
}

macro_rules! empty0 {
    () => ( f() );
}

macro_rules! empty1 {
    {} => { f() };
}

macro_rules! empty2 {
    [] => [ f() ];
}

// using multiple parens/brackets/curlies variants allows us to make sure we
// parse everything properly
fn main() {
    empty0!();
    empty1!{};
    empty2![];
}
