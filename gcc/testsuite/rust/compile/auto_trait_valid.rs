// #![feature(auto_traits)] // not present in Rust 1.49 yet

#![feature(optin_builtin_traits)]

auto trait MegaSend {}
pub auto trait MegaSync {}
unsafe auto trait SuperSync {}
pub unsafe auto trait SuperSend {}

fn main() {}
