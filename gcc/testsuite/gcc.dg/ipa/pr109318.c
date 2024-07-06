/* { dg-do run } */
/* { dg-options "-O2 -fno-early-inlining" } */
/* { dg-require-effective-target int32plus } */

#pragma pack(1)
struct S {
  signed : 31;
  unsigned f4 : 20;
};

static struct S global;

static struct S func_16(struct S *ptr) { return *ptr; }

int
main()
{
  struct S *local = &global;
  *local = func_16(local);
  return 0;
}
