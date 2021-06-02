fn main() {
    'outer: loop {
        'inner: loop {
            break 'outer;
        }
    }
}
