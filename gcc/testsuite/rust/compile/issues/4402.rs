#![feature(no_core)]
#![no_core]
use issue_4402_foo::Bar;
pub mod issue_4402_foo;

fn main() {
    // use '_a' to silence the unused variable warning
    let _a = Bar;
}