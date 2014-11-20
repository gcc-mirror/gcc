/* Check that loops whose final iteration is undefined are detected.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Waggressive-loop-optimizations" } */

void doSomething(char);

char array1[5];
char array2[5];

void
foo (int p)
{
  int i;
  for (i=0;
       (p
        ? array1[i]  /* { dg-message "note: possible undefined statement is here" } */
        : array2[i]) /* { dg-message "note: possible undefined statement is here" } */
       && i < 5      /* { dg-warning "loop exit may only be reached after undefined behavior" } */
       && i < 100;   /* { dg-warning "loop exit may only be reached after undefined behavior" } */
       i++)
    doSomething(array1[i]);
}
