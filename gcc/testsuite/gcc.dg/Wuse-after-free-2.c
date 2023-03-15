/* PR middle-end/104232 - spurious -Wuse-after-free after conditional free
   { dg-do compile }
   { dg-options "-O2 -Wall -fno-tree-loop-distribute-patterns -fno-ivopts" }  */

void free (void*);

void sink (void*);

void nowarn_cond_2 (char *p0, char *q0, int i)
{
  char *r = i ? p0 : q0;

  free (p0);

  /* The use of a PHI operand could be diagnosed using the "maybe" form
     of the warning at level 2 but it's not done.  If it ever changes
     this test and those below will need to be updated.  */
  sink (r);
}

void nowarn_cond_2_null (char *p0, int i)
{
  char *r = i ? p0 : 0;

  free (p0);
  sink (r);
}

void nowarn_cond_3 (char *p0, char *q0, int i)
{
  char *r = i < 0 ? p0 - 1 : 0 < i ? p0 + 1 : q0;

  free (p0);
  sink (r);
}

void nowarn_cond_3_null (char *p0, int i)
{
  char *r = i < 0 ? p0 - 1 : 0 < i ? p0 + 1 : 0;

  free (p0);
  sink (r);
}

void nowarn_cond_4 (char *p0, char *q0, int i)
{
  char *r = i < -1 ? p0 - 2 : i < 0 ? p0 - 1 : 1 < i ? p0 + 1 : q0;

  free (p0);
  sink (r);
}

int nowarn_cond_loop (char *p)
{
  char *q = p;
  while (*q)
    {
      if (*q == 'x')
        {
          q = "";
          break;
        }
      ++q;
    }

  free (p);
  return *q;
}


void warn_cond_2_cst (char *p, int i)
{
  /* Same as nowarn_cond_2() above but with R being derived only from
     P, which means that any R's use after P has been freed should be
     diagnosed.  */
  char *r = i ? p + 1 : p + 2;

  free (p);         // { dg-message "call to 'free'" }
  sink (r);         // { dg-warning "pointer 'r' used after 'free'" }
}

void warn_cond_2_var (char *p, int i, int j)
{
  char *r = i ? p + i : p + j;

  free (p);         // { dg-message "call to 'free'" }
  sink (r);         // { dg-warning "pointer 'r' used after 'free'" }
}

void warn_cond_3_var (char *p0, int i, int j)
{
  char *r = i < 0 ? p0 - i : 0 < i ? p0 + j : p0 + i + j;

  free (p0);        // { dg-message "call to 'free'" }
  sink (r + 1);     // { dg-warning "pointer 'r' used after 'free'" }
}

int warn_cond_4 (char *p0, char *q0, int i)
{
  char *r = i < -1 ? p0 - 2 : i < 0 ? p0 - 1 : 1 < i ? p0 + 2 : p0 + 1;

  free (p0);        // { dg-message "call to 'free'" }
  return *r;        // { dg-warning "pointer 'r' used after 'free'" }
}

int warn_cond_loop (char *p)
{
  char *q = p;

  /*  -fno-tree-loop-distribute-patterns ensures this does not get converted
      into rawmemchr (making q and p unrelated).  Also, -fno-ivopts is required
      for some targets, to not lose track of the pointer.  */
  while (*q)
    ++q;

  free (p);         // { dg-message "call to 'free'" }
  return *q;        // { dg-warning "pointer 'q' used after 'free'" }
}
