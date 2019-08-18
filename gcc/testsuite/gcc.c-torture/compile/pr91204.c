/* PR target/91204 */

int a, b, c[64];

void
foo (void)
{
  int i;
  for (i = 2; i < 64; i++)
    c[i] &= b ^ c[i] ^ c[i - 2];
}
