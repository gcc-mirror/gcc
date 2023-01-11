// { dg-output "invok\r*\ninvok\r*\ninvok\r*\ninvok\r*\ninvok\r*\n" }
extern "C" {
    fn printf(s: *const i8, ...);
}

fn f() {
    unsafe {
        let r_s = "invok\n\0";
        let s_p = r_s as *const str;
        let c_p = s_p as *const i8;

        printf(c_p);
    }
}

macro_rules! invocation0 {
    (valid) => {
        f();
    };
    () => {};
}

macro_rules! invocation1 {
    (valid) => {};
    () => {
        f();
    };
}

macro_rules! invocation2 {
    (valid) => {
        f();
    };
    (invalid) => {};
}

macro_rules! invocation3 {
    (this is a valid invocation) => {
        f();
    };
    (not this one) => {};
}

macro_rules! invocation4 {
    (fn f() {}) => {
        f();
    };
    (not a keyword) => {};
}

fn main() -> i32 {
    invocation0!(valid);
    invocation1!();
    invocation2!(valid);
    invocation3!(this is a valid invocation);
    invocation4!(
        fn f() {}
    );

    0
}
