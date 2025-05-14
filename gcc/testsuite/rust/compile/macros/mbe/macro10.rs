// { dg-additional-options "-w" }
macro_rules! foo {
    {} => {
        15
    };
}

fn main() {
    let a = foo!();
    let b = foo![];
}
