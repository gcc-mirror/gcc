// Test that qualifiers other than volatile are disallowed on asm.
// { dg-do compile }
// { dg-options "-std=gnu++98" }

void
f ()
{
  asm volatile ("");

  asm const (""); // { dg-error {'const' is not an asm qualifier} }

  asm __restrict (""); // { dg-error {'__restrict' is not an asm qualifier} }
}
