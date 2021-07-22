pub fn main() {
    let mut num = 2;
    let r1: *const i32 = &num;
    let r2 = unsafe { *r1 } + unsafe { *r1 };
    let r3 = num;
    num = 4;
    let r4 = num + unsafe { *r1 } * r3;
    let _eightteen = r2 + r3 + r4;
}
