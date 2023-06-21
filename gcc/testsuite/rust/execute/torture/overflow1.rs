// { dg-shouldfail "i8 overflow" }
// { dg-options "-fdump-tree-original" }

fn five() -> i8 {
    5
}

extern "C" {
    fn printf(fmt: *const i8, ...);
}

fn main() {
    let a = 127i8;
    let b = five();

    // { dg-final { scan-tree-dump ADD_OVERFLOW original } }
    let c = a + b;

    unsafe { printf("%d\n\0" as *const str as *const i8, c as i32) }
}
