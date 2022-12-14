// { dg-additional-options "-frust-crate=fancy_crate_name -fdump-tree-gimple" }
pub fn does_nothing() {}
fn main() {
    does_nothing()
}
// { dg-final { scan-tree-dump-times {fancy_crate_name::does_nothing} 2 gimple } }
// { dg-final { scan-tree-dump-times {fancy_crate_name::main} 1 gimple } }
