/* { dg-do compile } */
/* { dg-options "-O -ftrivial-auto-var-init=zero -Wuninitialized" } */

int
qy (void)
{
  int tw = 4;
  int fb[tw];
  return fb[2]; /* { dg-warning "uninitialized" } */
}
