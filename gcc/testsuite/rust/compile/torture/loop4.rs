#![feature(no_core)]
#![no_core]

fn main() {
    'outer: loop {
        'inner: loop {
            break 'outer;
        }
    }
}
