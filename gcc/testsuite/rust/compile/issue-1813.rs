fn main() {
    let a = 15u8;
    let a = &a;
    match a {
        &15 => {}
        &14 => {}
        _ => {}
    }
}
