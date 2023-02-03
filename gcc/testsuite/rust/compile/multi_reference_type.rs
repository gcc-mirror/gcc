fn main() {
    let a = 15u8;
    let a: &u8 = &a;
    let a: &&u8 = &a;
    let a: &&&u8 = &a;
    let _: &&&&u8 = &a;

    let _: &&u8;
    let _: &mut &u8;
    let _: &&mut u8;
    let _: &mut &mut &u8;
}
