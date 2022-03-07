// { dg-additional-options "-w" }
macro_rules! define_vars {
    ($($v:ident)*) => { $(let $v = 15;)* }
}

fn main() -> i32 {
    define_vars!(a0 b f __some_identifier);

    b
}
