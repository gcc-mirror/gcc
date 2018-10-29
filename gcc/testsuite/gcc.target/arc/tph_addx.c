/* { dg-do compile } */
/* { dg-options "-Os" } */

/* when compiling for size avoid the following peephole
-------------------------------------------------------------
Pattern 1 : r0 = r1 << {i}
            r3 = r4/INT + r0     ;;and commutative
                ||
                \/
            add{i} r3,r4/INT,r1
-------------------------------------------------------------
*/

typedef int a;
typedef int b                                                    ;
struct c
{
  b d;
};

struct e
{
  a f;
};

int g(int family)
{
  switch (family)
  case 2:
    return sizeof(struct e);
  return 0;
}

int h(int family)
{
  return 1 + g(family) - 1 ;
}

extern void m (void);

int i(int j)
{
  struct c *hdr;
  int k;
  int l;
  k = h(j);
  l = sizeof(struct c) +   k * 2;
  hdr->d = l ;
  if (j)
    m();
}

/* { dg-final { scan-assembler-not "add\d" } } */
