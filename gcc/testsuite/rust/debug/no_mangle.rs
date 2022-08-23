#[no_mangle]
fn do_not_mangle() -> i32 {
    0 
}

fn please_mangle() {}

fn main() {
// { dg-do compile }
// { dg-options "-gdwarf-5 -dA" }
    let _ = do_not_mangle();
    please_mangle();
// look for unmangled function name:
// { dg-final { scan-assembler "do_not_mangle:" } } */
// look for legacy mangled function name:
// { dg-final { scan-assembler "13please_mangle" } } */
}
