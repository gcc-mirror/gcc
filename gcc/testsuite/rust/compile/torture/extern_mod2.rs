// { dg-additional-options "-w" }

#[path = "modules/valid_path.rs"]
mod not_a_valid_path;

#[path ="modules/valid_path.rs"]
mod path_without_extra_equal;

#[path= "modules/valid_path.rs"]
mod no_leading_equal;

#[path       =     "modules/valid_path.rs"]
mod extra_spaces;

#[path] // { dg-error "path attributes must contain a filename" }
mod error; // { dg-error "no candidate found" }

// This is "valid", and should only error out when parsing
// the file
#[path = "not_a_valid_file.rs"]
mod another_error; // { dg-error "No such file or directory" }

fn main() {}
