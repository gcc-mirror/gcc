/* PR target/78904 */
/* { dg-do assemble { target { ! ia32 } } } */
/* { dg-options "-O2" } */

typedef __SIZE_TYPE__ size_t;

struct S1
{
  unsigned char pad1;
  unsigned char val;
  unsigned short pad2;
  unsigned int pad3;
};

extern struct S1 t[256];

struct S1 test_and (struct S1 a, size_t i)
{
  a.val &= t[i].val;

  return a;
}

struct S1 test_or (struct S1 a, size_t i)
{
  a.val |= t[i].val;

  return a;
}

struct S1 test_xor (struct S1 a, size_t i)
{
  a.val ^= t[i].val;

  return a;
}

struct S1 test_add (struct S1 a, size_t i)
{
  a.val += t[i].val;

  return a;
}
