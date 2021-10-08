/* { dg-do compile } */
/* { dg-options "-Os -fno-tree-vectorize -fdump-tree-optimized" } */


struct struct1
{
  void *data;
  unsigned short f1;
  unsigned short f2;
};
typedef struct struct1 S1;

struct struct2
{
  int f3;
  S1 f4;
};
typedef struct struct2 S2;


extern void foo (S1 *ptr);
extern S2 gstruct2_var;
extern S1 gstruct1_var;

static inline S1 bar (const S1 *ptr) __attribute__ ((always_inline));

static inline S1
bar (const S1 *ptr)
{
  S1 ls_var = *ptr;
  foo (&ls_var);
  return ls_var;
}

int
main ()
{
  S2 *ps_var;

  ps_var = &gstruct2_var;
  ps_var->f4 = bar (&gstruct1_var);

  return 0;
}
/* { dg-final { scan-tree-dump-times "short unsigned int\[^*\]*;" 0 "optimized"} } */
