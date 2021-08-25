#[path = "modules/valid_path.rs"]
mod not_a_valid_path;

#[path] // { dg-error "path attributes must contain a filename" }
mod error; // { dg-error "no candidate found for module error" }

// This is "valid", and should only error out when parsing
// the file
// FIXME: Add a dg-error directive on the `mod another_error` line once module expansion
// is added
#[path = "not_a_valid_file.rs"]
mod another_error;

fn main() {}
