// { dg-additional-options "-fdump-tree-gimple" }
#[cold]
fn cold_function() -> i32 {
    42
}

fn main() -> i32 {
    // { dg-final { scan-tree-dump-times {__attribute__\(\(cdecl, cold\)\)} 1 gimple } }
    cold_function();

    0
}
