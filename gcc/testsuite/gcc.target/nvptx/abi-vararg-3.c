/* { dg-do compile } */
/* { dg-additional-options "-Wno-pedantic -Wno-long-long" } */

/* 64-bit var args should be aligned to 64  bits.  */

void Foo (const char *, ...);

void Baz ()
{
  Foo ("", 0, 1ll);
}

/* { dg-final { scan-assembler "st.u64\t\\\[%stack\\+8\\\]," } } */
