macro_rules! call_f {
    ($($f:ident)*) => { $($f();)* }
}

fn f() {}

// This is valid and should parse items
fn main() {
    call_f!(f f f f);
}

