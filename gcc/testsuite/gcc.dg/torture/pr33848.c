/* &&foo should be hoisted, but on most targets, excess register pressure
   forces it to be rematerialized before "data != &&foo".  On targets that
   have a "branch if registers are equal" instruction, this leads to the
   branch having two LABEL_REFs: one for the branch target and one for
   &&foo.  When reloading &&foo into a register, reload would wrongly
   say that &&foo was the target of the branch, and the real target would
   then be removed as dead.  */
/* { dg-do link } */
#define NVARS 30
#define MULTI(X) \
  X( 0), X( 1), X( 2), X( 3), X( 4), X( 5), X( 6), X( 7), X( 8), X( 9), \
  X(10), X(11), X(12), X(13), X(14), X(15), X(16), X(17), X(18), X(19), \
  X(20), X(21), X(22), X(23), X(24), X(25), X(26), X(27), X(28), X(29)

#define DECLARE(INDEX) i##INDEX = gv[INDEX]
#define COPY(INDEX) gv[INDEX] = i##INDEX

volatile int gv[NVARS];
void *volatile data;

int
main (void)
{
  __label__ foo;

  if (gv[0] == 1)
    goto foo;
  data = &&foo;
  do
    {
      int MULTI (DECLARE);
      MULTI (COPY);
      MULTI (COPY);
      MULTI (COPY);
      if (data != &&foo)
	gv[0] = 1;
      else
	gv[1] = 2;
    }
  while (gv[0] > 0);
 foo:
  return 0;
}
