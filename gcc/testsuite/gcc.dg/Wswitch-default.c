/* { dg-do compile } */
/* { dg-options "-Wswitch-default" } */

enum e { e1, e2 };

int
foo (int i, int j, enum e ei, enum e ej, enum e ek, enum e el,
     enum e em, enum e en, enum e eo, enum e ep)
{
  switch (i)
    {
    case 1: return 1;
    case 2: return 2;
    } /* { dg-warning "switch missing default case" } */
  switch (j)
    {
    case 3: return 4;
    case 4: return 3;
    default: break;
    }
  switch (ei)
    { /* { dg-warning "switch missing default case" } */
    }
  switch (ej)
    {
    default: break;
    }
  switch (ek)
    {
    case e1: return 1;
    } /* { dg-warning "switch missing default case" } */
  switch (el)
    {
    case e1: return 1;
    default: break;
    }
  switch (em)
    {
    case e1: return 1;
    case e2: return 2;
    } /* { dg-warning "switch missing default case" } */
  switch (en)
    {
    case e1: return 1;
    case e2: return 2;
    default: break;
    }
  switch (eo)
    {
    case e1: return 1;
    case e2: return 2;
    case 3: return 3;
    } /* { dg-warning "switch missing default case" } */
  switch (ep)
    {
    case e1: return 1;
    case e2: return 2;
    case 3: return 3;
    default: break;
    }
  return 0;
}
