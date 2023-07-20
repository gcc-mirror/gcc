// { dg-additional-options "-frust-crate-type=proc-macro" }

pub fn wild_pub_module() {} // { dg-error ".proc-macro. crate types currently cannot export any items other than functions tagged with .#.proc_macro.., .#.proc_macro_derive.. or .#.proc_macro_attribute.." }
