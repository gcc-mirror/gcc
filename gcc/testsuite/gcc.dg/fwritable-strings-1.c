/* PR c/12818 */
/* Origin: <fnf@ninemoons.com> */

/* { dg-do run } */
/* { dg-options "-fwritable-strings" } */
/* { dg-error "-fwritable-strings is deprecated" "" { target *-*-* } 0 } */

extern void abort(void);

char *names[] = {"alice", "bob", "john"};

int main (void)
{
  if (names[1][0] != 'b')
    abort();

  return 0;
}
