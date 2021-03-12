/* PR sanitizer/99168 */
/* { dg-do run } */

struct my_struct
{
  unsigned long volatile x;
} __attribute__((aligned(128)));

static int variablek[5][6] = {};
static struct my_struct variables1 = {0UL};
static struct my_struct variables2 __attribute__((aligned(32))) = {0UL};

int main() {
  int i, j;
  for (i = 0; i < 5; i++) {
    for (j = 0; j < 6; j++) {
      __builtin_printf("%d ", variablek[i][j]);
    }
  }
  __builtin_printf("\n");

  __builtin_printf("%lu\n", variables1.x);
  __builtin_printf("%lu\n", variables2.x);

  return 0;
}
