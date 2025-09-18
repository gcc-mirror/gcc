pub fn print(a: *const u8) {}
#[macro_export]
macro_rules! pr_warn (
    ($($arg:tt)*) => (
        $($crate::print($arg))*
    )
);

fn main() {
    pr_warn!("test\0", "test\0");
    // { dg-error "expecting .;. but .identifier. found" "" { target *-*-* } .-1 }
}
