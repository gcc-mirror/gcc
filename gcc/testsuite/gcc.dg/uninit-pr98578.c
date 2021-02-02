/* PR middle-end/98578 - ICE warning on uninitialized VLA access
   { dg-do compile }
   { dg-options "-O2 -Wall" } */

void* malloc (__SIZE_TYPE__);

void T (int, ...);

void vla_n (int n, int i)
{
  int a1[n];

  /* a1[I] should be formatted as as a1[I] (or, for I == 0, perhaps
     as *a1), but definitely not as *a1[I].  This is a bug in VLA
     formatting.  */
  T (a1[0]);        // { dg-warning "'a1\\\[0]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "'\\*a1\\\[0]' is used uninitialized" "spurious star" { target *-*-* } .-1 }
  T (a1[1]);        // { dg-warning "a1\\\[1]' is used uninitialized" }
  T (a1[i]);        // { dg-warning "a1\\\[i]' is used uninitialized" }
}

void vla_n_2 (int n, int i)
{
  int a2[n][2];

  T (a2[0][0]);   // { dg-warning "a2\\\[0]\\\[0]' is used uninitialized" }
  T (a2[2][1]);   // { dg-warning "a2\\\[2]\\\[1]' is used uninitialized" }
  T (a2[3][i]);   // { dg-warning "a2\\\[3]\\\[i]' is used uninitialized" }
  T (a2[i][0]);   // { dg-warning "a2\\\[i]\\\[0]' is used uninitialized" }
  T (a2[i][i]);   // { dg-warning "a2\\\[i]\\\[i]' is used uninitialized" }
}


void vla_3_n (int n, int i)
{
  int a2[3][n];

  T (a2[0][0]);     // { dg-warning "a2\\\[0]\\\[0]' is used uninitialized" }
  T (a2[1][2]);     // { dg-warning "a2\\\[1]\\\[2]' is used uninitialized" }
  T (a2[2][i]);     // { dg-warning "a2\\\[2]\\\[i]' is used uninitialized" }
  T (a2[i][3]);     // { dg-warning "a2\\\[i]\\\[3]' is used uninitialized" }
  T (a2[i][i]);     // { dg-warning "a2\\\[i]\\\[i]' is used uninitialized" }
}


void vla_n_n (int n, int i)
{
  int a2[n][n];

  T (a2[0][0]);     // { dg-warning "a2\\\[0]\\\[0]' is used uninitialized" }
  T (a2[4][5]);     // { dg-warning "a2\\\[4]\\\[5]' is used uninitialized" }
  T (a2[6][i]);     // { dg-warning "a2\\\[6]\\\[i]' is used uninitialized" }
  T (a2[i][7]);     // { dg-warning "a2\\\[i]\\\[7]' is used uninitialized" }
  T (a2[i][i]);     // { dg-warning "a2\\\[i]\\\[i]' is used uninitialized" }
}


void char_ptr_n (int n, int i)
{
  char *p = malloc (n);

  T (p[0]);         // { dg-warning "'\\\*p' is used uninitialized" }
  T (p[1]);         // { dg-warning "'p\\\[1]' is used uninitialized" }
  T (p[i]);         // { dg-warning "'p\\\[i]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "is used uninitialized" "POINTER_PLUS_EXPR" { target *-*-* } .-1 }
}


void int_ptr_n (int n, int i)
{
  int *p = malloc (n);

  T (p[0]);         // { dg-warning "'\\\*p' is used uninitialized" }
  T (p[1]);         // { dg-warning "'p\\\[1]' is used uninitialized" }
  T (p[i]);         // { dg-warning "'p\\\[i]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "is used uninitialized" "POINTER_PLUS_EXPR" { target *-*-* } .-1 }
}


void int_arr_ptr_n (int n, int i)
{
  int (*p)[n] = malloc (n);

  T ((*p)[0]);      // { dg-warning "\\(\\*p\\)\\\[0]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "\\*p\\\[0]' is used uninitialized" "missing parens" { target *-*-* } .-1 }
  T ((*p)[1]);      // { dg-warning "\\(\\*p\\)\\\[1]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "\\*p\\\[1]' is used uninitialized" "missing parens" { target *-*-* } .-1 }
  T ((*p)[i]);      // { dg-warning "\\(\\*p\\)\\\[i]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "\\*p\\\[i]' is used uninitialized" "missing parens" { target *-*-* } .-1 }
}


void int_arr_ptr_n_n (int n, int i)
{
  int (*p)[n][n] = malloc (n);

  T ((*p)[0][0]);   // { dg-warning "\\(\\*p\\)\\\[0]\\\[0]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "\\*p\\\[0]\\\[0]' is used uninitialized" "missing parens" { target *-*-* } .-1 }
  T ((*p)[1][2]);   // { dg-warning "\\(\\*p\\)\\\[1]\\\[2]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "\\*p\\\[1]\\\[2]' is used uninitialized" "missing parens" { target *-*-* } .-1 }
  T ((*p)[0][i]);   // { dg-warning "\\(\\*p\\)\\\[0]\\\[i]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "\\*p\\\[0]\\\[i]' is used uninitialized" "missing parens" { target *-*-* } .-1 }
  T ((*p)[3][i]);   // { dg-warning "\\(\\*p\\)\\\[3]\\\[i]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "\\*p\\\[3]\\\[i]' is used uninitialized" "missing parens" { target *-*-* } .-1 }
  T ((*p)[i][i]);   // { dg-warning "\\(\\*p\\)\\\[i]\\\[i]' is used uninitialized" "pr98587" { xfail *-*-* } }
                    // { dg-warning "\\*p\\\[i]\\\[i]' is used uninitialized" "missing parens" { target *-*-* } .-1 }

  T ((*p)[i][i + 1]); // { dg-warning "\\(\\*p\\)\\\[i]\\\[i \\+ 1]' is used uninitialized" "pr98588" { xfail *-*-* } }
                    // { dg-warning "\\*p\\\[i]\\\[<unknown>]' is used uninitialized" "missing parens" { target *-*-* } .-1 }
}
