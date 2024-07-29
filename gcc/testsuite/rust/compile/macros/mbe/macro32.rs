macro_rules! s {
    ($s:stmt) => {{}};
}

macro_rules! multi_s {
    ($($s:stmt)+) => {{}};
}

fn main() -> i32 {
    s!(let a = 15);
    s!(;); // Empty statement
    s!(let a = 15;); // { dg-error "Failed to match any rule within macro" }
    multi_s!(let a = 15;);
    // ^ this actually gets parsed as two statements - one LetStmt and one
    // empty statement. This is the same behavior as rustc, which you can
    // see using a count!() macro

    32
}
