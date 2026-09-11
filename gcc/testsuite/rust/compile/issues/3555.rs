#![feature(no_core)]
#![no_core]

fn main() {
    match SomeStruct(2) {
        StructConst1(_) => {}

        _ => {}
    }

    struct SomeStruct(u8);

    const StructConst1: SomeStruct = SomeStruct(1);
}
