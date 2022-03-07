macro_rules! zero_or_one {
    ($($a:literal)?) => { // { dg-error "invalid amount of matches for macro invocation. Expected between 0 and 1, got 2" }
        f();
    }
}

fn main() {
    zero_or_one!();
    zero_or_one!(14);
    zero_or_one!(125 12 "gcc"); // { dg-error "Failed to match any rule within macro" }
}
