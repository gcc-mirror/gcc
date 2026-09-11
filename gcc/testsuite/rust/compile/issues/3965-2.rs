#![feature(no_core)]
#![no_core]

fn main() {
    loop { continue }

    [(); {while true {break}; 0}];

    [(); {while true {break}; 0}];
}
