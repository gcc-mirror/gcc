// { dg-additional-options "-fdump-tree-gimple" }
#![crate_name = "specified_name"]
// { dg-final { scan-tree-dump-times {specified_name::main} 1 gimple } }
fn main() {}
