/* { dg-do compile } */
/* { dg-options "-march=rv64imafd_zicsr_zifencei_zca_zcmp -mabi=lp64d -Os" } */
/* { dg-skip-if "" { *-*-* } {"-O0" "-O1" "-O2" "-Og" "-O3" "-Oz" "-flto"} } */

typedef struct
{
  struct
  {
    struct
    {
      struct
      {
	long a;
      };
    } a[129];
  };
} b;

struct c
{
  void *a[129];
};

extern void
f (struct c, b *);

struct c
d ()
{
  struct c a;
  __builtin_unwind_init ();
  b e;
  f (a, &e);
}
