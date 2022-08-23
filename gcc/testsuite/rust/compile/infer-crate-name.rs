// { dg-additional-options "-fdump-tree-gimple" }
pub fn does_nothing() {}
fn main() {
    does_nothing()
}
// { dg-final { scan-tree-dump-times {infer_crate_name::does_nothing} 2 gimple } }
// { dg-final { scan-tree-dump-times {infer_crate_name::main} 1 gimple } }
