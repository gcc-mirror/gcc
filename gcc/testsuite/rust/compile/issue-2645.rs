// { dg-additional-options "-frust-compile-until=ast" }
pub fn struct_tuple(A { 0: a, 1: ref b }: A) -> i32 {
    a
}
