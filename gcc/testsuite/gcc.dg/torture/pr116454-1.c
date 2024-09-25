/* { dg-do run } */
/* { dg-additional-options "-ffast-math" } */

static int t = 0;
_Complex float f()
{
        t++;
        return 0;
}
int main() {
	t = 0;
	/* Would cause f() to be incorrectly invoked twice. */
	f() * 1j;
	if (t != 1)
          __builtin_abort();
}
