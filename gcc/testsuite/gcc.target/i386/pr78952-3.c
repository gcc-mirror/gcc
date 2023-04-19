/* PR target/78952 */
/* { dg-do compile } */
/* { dg-require-effective-target nonpic } */
/* { dg-options "-O2 -masm=att" } */
/* { dg-additional-options "-mregparm=1" { target ia32 } } */
/* { dg-final { scan-assembler-not "mov\[sz]bl" } }  */

typedef __SIZE_TYPE__ size_t;

struct S1s
{
  char pad1;
  char val;
  short pad2;
};

extern char ts[256];

_Bool foo (struct S1s a, size_t i)
{
  return (ts[i] > a.val);
}

/* { dg-final { scan-assembler "cmpb\[ \\t]+ts\[^\n]*%.h" } }  */

struct S1u
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
};

extern unsigned char tu[256];

_Bool bar (struct S1u a, size_t i)
{
  return (tu[i] > a.val);
}

/* { dg-final { scan-assembler "cmpb\[ \\t]+tu\[^\n]*%.h" } }  */
