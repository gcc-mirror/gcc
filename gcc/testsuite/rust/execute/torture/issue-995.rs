struct Pattern(i32);

fn pattern_as_arg(Pattern(value): Pattern) -> i32 {
    value
}

fn main() -> i32 {
    pattern_as_arg(Pattern(15)) - 15
}
