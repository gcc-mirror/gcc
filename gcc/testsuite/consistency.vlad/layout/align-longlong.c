#include <stdio.h>

static long long ll;

int main(void)
{
  printf ("+++Long long alignment:\n");
  printf ("%d\n", __alignof__ (ll));
  return 0;
}
