/* PR middle-end/96963 - -Wstringop-overflow false positive with
   -ftree-vectorize when assigning consecutive char struct members
   { dg-do compile }
   { dg-options "-O2 -Wall -ftree-vectorize" } */

void sink (void*);

struct Char
{
  int i;
  char c, d, e, f;
  char a[2], b[2];
};

void nowarn_char_assign (struct Char *p)
{
  sink (&p->c);

  /* Verify the bogus warning triggered by the tree-ssa-strlen.c pass
     is not issued.  */
  p->c = 1;         // { dg-bogus "\\\[-Wstringop-overflow" }
  p->d = 2;
  p->e = 3;
  p->f = 4;
}

void nowarn_char_array_assign (struct Char *p)
{
  sink (p->a);

  p->a[0] = 1;      // { dg-bogus "\\\[-Wstringop-overflow" }
  p->a[1] = 2;
  p->b[0] = 3;
  p->b[1] = 4;
}

void warn_char_array_assign_interior (struct Char *p)
{
  sink (p->a);

  p->a[0] = 1;
  p->a[1] = 2;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
  /* Warnings are only suppressed for trailing arrays.  Verify
     one is issued for an interior array.  */
  p->a[2] = 5;      // { dg-warning "\\\[-Wstringop-overflow" }
#pragma GCC diagnostic pop
}

void warn_char_array_assign_trailing (struct Char *p)
{
  /* This is separated from warn_char_array_assign_interior because
     otherwise GCC removes the store to p->a[2] as dead since it's
     overwritten by p->b[0].  */
  sink (p->b);

  p->b[0] = 3;
  p->b[1] = 4;
#pragma GCC diagnostic push
#pragma GCC diagnostic ignored "-Warray-bounds"
  /* Warnings are only suppressed for trailing arrays with at most
     one element.  Verify one is issued for a two-element array.  */
  p->b[2] = 5;      // { dg-warning "\\\[-Wstringop-overflow" }
#pragma GCC diagnostic pop
}


/* Also verify there's no warning for other types than char (even though
   the problem was limited to chars and -Wstringop-overflow should only
   trigger for character accesses).  */

struct Short
{
  int i;
  short c, d, e, f;
  short a[2], b[2];
};

void nowarn_short_assign (struct Short *p)
{
  sink (&p->c);

  p->c = 1;
  p->d = 2;
  p->e = 3;
  p->f = 4;
}

void nowarn_short_array_assign (struct Short *p)
{
  sink (p->a);

  p->a[0] = 1;
  p->a[1] = 2;
  p->b[0] = 3;
  p->b[1] = 4;
}
