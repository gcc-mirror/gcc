// { dg-additional-options "-w" }

#[path = "modules/valid_path.rs"]
mod not_a_valid_path;

// #[path]
// FIXME: This is wrong
// mod error; 

// This is "valid", and should only error out when parsing
// the file
// FIXME: Fix path attribute expanding
// #[path = "not_a_valid_file.rs"]
// mod another_error;

fn main() {}
