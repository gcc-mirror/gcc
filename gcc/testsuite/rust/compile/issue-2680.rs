// { dg-additional-options "-fdump-tree-gimple" }
pub fn test_cast() {
    let i = 1;
    // { dg-final { scan-tree-dump-times {const i32 i;} 1 gimple } }
    let _j = i as i64;
}
