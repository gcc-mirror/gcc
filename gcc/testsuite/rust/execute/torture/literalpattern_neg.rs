fn main() -> i32 {
    let x = -55;

    match x {
        55 => 1,
        -55 => 0, // correct case
        _ => 1
    }
}