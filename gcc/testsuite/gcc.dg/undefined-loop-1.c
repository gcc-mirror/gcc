/* Check that loops whose final iteration is undefined are detected.  */
/* { dg-do compile } */
/* { dg-options "-O2 -Waggressive-loop-optimizations" } */

void doSomething(char);

char array[5];

void
foo (void)
{
  int i;
  for (i = 0;
       array[i]  /* { dg-message "note: possible undefined statement is here" } */
       && i < 5; /* { dg-warning "loop exit may only be reached after undefined behavior" } */
       i++)
    doSomething(array[i]);
}
