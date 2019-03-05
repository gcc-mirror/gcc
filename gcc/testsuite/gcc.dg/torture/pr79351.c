/* { dg-do run } */

static struct state {
    int k;
    int dummy;
} states[256];

__attribute((noipa))
static void
ismatch(int n)
{
  for (int j=0; j<n; j++) {
      states[j] = (struct state){0};
      for (int i = 0; i <= j; i++) {
	  states[i].k++;
      }
  }
}

int
main()
{
  ismatch(2);
  if (states[0].k != 2)
    __builtin_abort();
}
