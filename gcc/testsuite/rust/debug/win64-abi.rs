pub extern "win64" fn square(num: i32) -> i32 {
    num * num
}

fn main() {
    // { dg-do compile { target { x86_64-*-* } } }
    // { dg-options "-gdwarf-5 -dA -w -O1" }
    // MS ABI dictates that the first argument is ecx instead of edi from the sysv world
    // { dg-final { scan-assembler "%ecx, %ecx" } }
    square(1);
}
