#![feature(no_core)]
#![no_core]

fn main() {
    'label: while break 'label {}
}
