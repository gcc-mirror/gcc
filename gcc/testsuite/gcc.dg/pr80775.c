/* PR middle-end/80775 ICE: -O3 produces ice in group_case_labels_stmt.  */
/* { dg-do compile }  */
/* { dg-options "-O3" }  */

typedef struct { short a; } b;
b c[10];
int d, e, f, g, h;
int
i (void)
{
  f = 0;
  for (; f < e; f++) {
    switch (g) {
    case 1:
      d = 1;
    case 2:
      c[2 + f].a = 1;
    }
    h += c[f].a;
  }
}
