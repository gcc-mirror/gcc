enum Enum {
    Unit,
    Tuple(i32),
    Struct { x: i32 },
}

fn main() {
    Enum::Unit {};
}