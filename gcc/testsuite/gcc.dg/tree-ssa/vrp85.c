/* PR tree-optimization/54810 */
/* { dg-do link } */
/* { dg-options "-O2 -fdump-tree-vrp1" } */

extern void link_error (void);

#define T(n, ntype, wtype) \
void				\
f##n (wtype s)			\
{				\
  if ((ntype) s == 0)		\
    return;			\
  if (s == 0)			\
    link_error ();		\
}

T(1, unsigned char, unsigned char)
T(2, unsigned char, unsigned short)
T(3, unsigned char, unsigned int)
T(4, unsigned char, unsigned long int)
T(5, unsigned char, unsigned long long int)
T(6, unsigned short int, unsigned short int)
T(7, unsigned short int, unsigned int)
T(8, unsigned short int, unsigned long int)
T(9, unsigned short int, unsigned long long int)
T(10, unsigned int, unsigned int)
T(11, unsigned int, unsigned long int)
T(12, unsigned int, unsigned long long int)
T(13, unsigned long int, unsigned long int)
T(14, unsigned long int, unsigned long long int)
T(15, unsigned long long int, unsigned long long int)

int
main ()
{
  return 0;
}

/* { dg-final { scan-tree-dump-not "link_error" "vrp1"} } */
/* { dg-final { cleanup-tree-dump "vrp1" } } */
