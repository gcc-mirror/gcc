// Output of statement macros is always parsed as a statement, so no semicolon
// is needed on the inner macro.

macro_rules! m {
    (macro) => { m!(stmts) };
    (stmts) => { let x = 3; x - 3 }
}

fn foo() -> i32 {
    m!{macro}
}

fn main() -> i32 {
    foo()
}
