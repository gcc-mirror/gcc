// { dg-additional-options "-fdump-tree-gimple" }
#[inline]
fn test_a() {}

// { dg-final { scan-tree-dump-times {always_inline} 1 gimple } }
#[inline(always)]
fn test_b() {}

#[inline(never)]
fn test_c() {}

fn main() {
    test_a();
    test_b();
    test_c();
}
