macro_rules! rep {
    ($a:literal) => { $a }; // { dg-error "reached recursion limit" }
    ($a:literal $(, $e:literal)*) => { // { dg-error "reached recursion limit" }
        $a + rep!(0 $(, $e)*) // { dg-error "Failed to match" }
    }
}

fn main() -> i32 {
    rep!(1, 2)
}
