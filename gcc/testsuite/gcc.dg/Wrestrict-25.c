/* PR tree-optimization/105604  - ICE: in tree_to_shwi with vla in struct
   and sprintf
   { dg-do compile }
   { dg-options "-O2 -Wall -Wrestrict" } */

extern int sprintf (char*, const char*, ...);

void* sink (void*);


void sprintf_S_a8_an_bn (int n, int i, int j)
{
  struct {
    char a8[8], an[n], bn[n];
  } *p = sink (0);

  {
    char *d = p->a8 + i;
    char *s = p->a8;
    sprintf (d, "%s", s);       // { dg-warning "argument 3 may overlap" }
    sink (p);
  }

  {
    char *d = p->a8;
    char *s = p->a8 + j;
    sprintf (d, "%s", s);       // { dg-warning "argument 3 may overlap" }
    sink (p);
  }

  {
    char *d = p->a8 + i;
    char *s = p->a8 + j;
    sprintf (d, "%s", s);       // { dg-warning "argument 3 may overlap" }
    sink (p);
  }

  {
    char *d = p->a8 + i;
    char *s = p->an;
    sprintf (d, "%s", s);
    sink (p);
  }

  {
    char *d = p->a8;
    char *s = p->an + j;
    sprintf (d, "%s", s);
    sink (p);
  }

  {
    char *d = p->a8 + i;
    char *s = p->an + j;
    sprintf (d, "%s", s);
    sink (p);
  }

  {
    /* The IL makes it impossible to rule out an overlap between
       p->a8 + i and p->bn + i so the "may overlap" warning triggers.  */
    char *d = p->a8 + i;
    char *s = p->bn;
    sprintf (d, "%s", s);       // { dg-bogus "-Wrestrict" "pr??????" { xfail *-*-* } }
    sink (p);
  }

  {
    char *d = p->a8;
    char *s = p->bn + j;
    sprintf (d, "%s", s);       // { dg-bogus "-Wrestrict" "pr??????" { xfail *-*-* } }
    sink (p);
  }

  {
    char *d = p->a8 + i;
    char *s = p->bn + j;
    sprintf (d, "%s", s);       // { dg-bogus "-Wrestrict" "pr??????" { xfail *-*-* } }
    sink (p);
  }

  {
    char *d = p->an + i;
    char *s = p->bn;
    sprintf (d, "%s", s);
    sink (p);
  }

  {
    char *d = p->an;
    char *s = p->bn + j;
    sprintf (d, "%s", s);
    sink (p);
  }

  {
    char *d = p->an + i;
    char *s = p->bn + j;
    sprintf (d, "%s", s);
    sink (p);
  }

  {
    char *d = p->an + i;
    char *s = p->a8;
    sprintf (d, "%s", s);
    sink (p);
  }

  {
    char *d = p->an;
    char *s = p->a8 + j;
    sprintf (d, "%s", s);
    sink (p);
  }

  {
    char *d = p->an + i;
    char *s = p->a8 + j;
    sprintf (d, "%s", s);
    sink (p);
  }

  {
    char *d = p->bn + i;
    char *s = p->a8;
    sprintf (d, "%s", s);       // { dg-bogus "-Wrestrict" "pr??????" { xfail *-*-* } }
    sink (p);
  }

  {
    char *d = p->bn;
    char *s = p->a8 + j;
    sprintf (d, "%s", s);       // { dg-bogus "-Wrestrict" "pr??????" { xfail *-*-* } }
    sink (p);
  }

  {
    char *d = p->bn + i;
    char *s = p->a8 + j;
    sprintf (d, "%s", s);       // { dg-bogus "-Wrestrict" "pr??????" { xfail *-*-* } }
    sink (p);
  }

  {
    char *d = p->bn + i;
    char *s = p->bn;
    sprintf (d, "%s", s);       // { dg-warning "may overlap" }
    sink (p);
  }

  {
    char *d = p->bn;
    char *s = p->bn + j;
    sprintf (d, "%s", s);       // { dg-warning "may overlap" }
    sink (p);
  }

  {
    char *d = p->bn + i;
    char *s = p->bn + j;
    sprintf (d, "%s", s);       // { dg-warning "may overlap" }
    sink (p);
  }
}
