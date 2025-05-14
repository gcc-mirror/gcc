macro_rules! zero_or_one {
    ($($a:literal)?) => {
        1 + 2;
    }
}

fn main() {
    zero_or_one!();
    zero_or_one!(14);
    zero_or_one!(125 12 "gcc"); // { dg-error "Failed to match any rule within macro" }
}
