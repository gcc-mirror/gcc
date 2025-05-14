/* PR middle-end/63272 - GCC should warn when using pointer to dead scoped
   variable within the same function
   Exercise -Wdangling-pointer for VLAs.
   { dg-do compile }
   { dg-options "-O0 -Wall -ftrack-macro-expansion=0" } */

void sink (void*, ...);

void nowarn_vla (int n)
{
  {
    int vla1[n];
    int *p1 = vla1;
    sink (p1);

    {
      int vla2[n];
      int *p2 = vla2;
      sink (p1, p2);

      {
	int vla3[n];
	int *p3 = vla3;
	sink (p1, p2, p3);
      }
      sink (p1, p2);
    }
    sink (p1);
  }
}

void warn_one_vla (int n)
{
  int *p;
  {
    int vla[n];               // { dg-message "'vla' declared" "pr??????" { xfail *-*-* } }
    p = vla;
  }
  sink (p);                   // { dg-warning "using a dangling pointer to 'vla'" "vla" { xfail *-*-* } }
}


void warn_two_vlas_same_block (int n)
{
  int *p, *q;
  {
    int vla1[n];              // { dg-message "'vla1' declared" "pr??????" { xfail *-*-* } }
    int vla2[n];              // { dg-message "'vla2' declared" "pr??????" { xfail *-*-* } }
    p = vla1;
    q = vla2;
  }

  sink (p);                   // { dg-warning "using a dangling pointer to 'vla1'" "vla" { xfail *-*-* } }
  sink (q);                   // { dg-warning "using a dangling pointer to 'vla2'" "vla" { xfail *-*-* } }
}


void warn_two_vlas_in_series (int n)
{
  int *p;
  {
    int vla1[n];              // { dg-message "'vla1' declared" "pr??????" { xfail *-*-* } }
    p = vla1;
  }
  sink (p);                   // { dg-warning "using a dangling pointer to 'vla1'" "vla" { xfail *-*-* } }

  int *q;
  {
    int vla2[n];              // { dg-message "'vla2' declared" "pr??????" { xfail *-*-* } }
    q = vla2;
  }
  sink (q);                   // { dg-warning "using a dangling pointer to 'vla2'" "vla" { xfail *-*-* } }
}
