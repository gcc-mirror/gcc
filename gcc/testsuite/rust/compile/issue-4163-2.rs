enum Enum {
    NotEmpty {x: i32},
    Struct {},
    Tuple (),
}

fn main() {
    Enum::Struct {};
    Enum::Tuple {};
}