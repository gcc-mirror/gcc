// { dg-additional-options "-w" }
macro_rules! create_type {
    ($s:ident) => {
        struct $s;
    };
}

fn main() {
    create_type!(A);

    let a = A;
}
