/* { dg-do run } */
/* { dg-options "-O2" } */
/* { dg-require-effective-target int32plus } */

struct tree_decl
{
  union tree_decl_u1 {
    int f;
    long i;
    struct tree_decl_u1_a {
      unsigned int align : 24;
      unsigned int off_align : 8;
    } a;
  } u1;
};

extern void abort (void);

unsigned int
assemble_variable (struct tree_decl decl)
{
  unsigned int align;

  align = decl.u1.a.align;

  if (align > (1 << ((8 * 4) < 64 ? (8 * 4) - 2 : 62)))
    align = (1 << ((8 * 4) < 64 ? (8 * 4) - 2 : 62));

  /* VRP should not be propagating 0 to the RHS of this assignment.
     But it was erroneously applying a cast operation between types of
     different precision.  align is an unsigned int with range [0,
     0x4000000] but the .align field holds only 24 bits.  So the cast
     was returning a [0, 0] range.  */
  decl.u1.a.align = align;

  return decl.u1.a.align;
}

main ()
{
  struct tree_decl decl;
  decl.u1.a.align = 13;
  unsigned int x = assemble_variable (decl);
  if (x == 0)
    abort ();
  return 0;
}
