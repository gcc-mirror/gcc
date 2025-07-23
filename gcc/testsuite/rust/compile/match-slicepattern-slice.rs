fn main() {
    let arr = [1, 2];
    let slice: &[i32] = &arr;

    match slice {
        [1] => {},
        [_, 2] => {},
        _ => {}
    }
}