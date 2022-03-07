fn f() {}

macro_rules! one_or_more {
    ($($a:literal)+) => { // { dg-error "invalid amount of matches for macro invocation" }
        f();
    };
}

fn main() {
    one_or_more!(1 1 1 1 1 1 1 1 1 1 1 "rust" 'c');
    one_or_more!(1);
    one_or_more!(); // { dg-error "Failed to match any rule within macro" }
}
