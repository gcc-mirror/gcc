// { dg-additional-options "-w" }
const TEST: *mut u8 = 123 as *mut u8;

fn test() {
    let a = TEST;
}
