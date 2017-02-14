/* Make sure strings are recognized as being accessible through larl.
   This requires the symbol ref alignment properly propagated to
   encode_section_info.  */

/* { dg-do compile } */
/* { dg-options "-march=z900 -O2 -fpic" } */


extern void foo(const char*, const char*, const char*);

void bar(int i)
{
  const char t1[10] = "test";
  const char t2[10] = "test2";
  const char t3[2][10] = {
       "foofoofoo",
       "barbarbar",
    };
  foo(t1, t2, t3[i]);
}

/* { dg-final { scan-assembler-not "GOTOFF" } } */
