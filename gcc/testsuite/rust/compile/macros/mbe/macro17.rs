macro_rules! rep {
    ($a:literal) => { $a };
    ($a:literal $(, $e:literal)*) => {
        $a + rep!(0 $(, $e)*) // { dg-error "reached recursion limit" }
    }
}

fn main() -> i32 {
    rep!(1, 2)
}
