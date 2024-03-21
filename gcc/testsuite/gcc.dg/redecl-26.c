/* We used to ICE while folding memcpy and memmove.
   PR c/109619. */
/* { dg-do compile } */
/* { dg-options "" } */

int *a1, *a2;

void foo(__SIZE_TYPE__ a3) /* { dg-note "" }  */
{
  __builtin_memcpy(a1, a2, a3);
  __builtin_memmove(a1, a2, a3);
  int *a3; /* { dg-error "redeclared as different kind of symbol" } */
}

