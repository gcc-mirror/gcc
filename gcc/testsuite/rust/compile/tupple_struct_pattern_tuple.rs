// { dg-additional-options "-frust-compile-until=typecheck" }
struct Struct(i32);

fn struct_pattern(Struct { 0: a }: Struct) {}
