// { dg-additional-options "-w" }
macro_rules! create_type {
    ($s:ident) => {
        struct $s;
    };
}

create_type!(SomeOuterType);

fn main() {
    let a = SomeOuterType;
}
