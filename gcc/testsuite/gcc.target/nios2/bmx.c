/* { dg-do compile } */
/* { dg-options "-O2 -march=r2 -mbmx" } */

/* Test generation of Nios II R2 BMX instructions.  */

struct s {
  unsigned int pad1 : 3;
  unsigned int bitfield : 20;
  unsigned int intfield;
};

void f (struct s *a, struct s *b)
{
  a->bitfield = b->bitfield;
}

void g (struct s *a, struct s *b)
{
  a->bitfield = b->intfield;
}

void h (struct s *a, struct s *b)
{
  a->intfield = b->bitfield;
}

/* { dg-final { scan-assembler "\tmerge\t.*, 22, 3" } }  */
/* { dg-final { scan-assembler "\tinsert\t.*, 22, 3" } }  */
/* { dg-final { scan-assembler "\textract\t.*, 22, 3" } }  */
