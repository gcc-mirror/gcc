macro_rules! unroll {
    {} => {}
}

pub fn foo() {
    let mut _a = 14;
    unroll! {}
    let mut _b = 13;
}

fn main() -> i32 { 0 }
