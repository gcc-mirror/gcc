/* PR target/28924 */

char c;

void
testc (void)
{
  (void) __sync_fetch_and_add (&c, -1);
}

short s;

void
tests (void)
{
  (void) __sync_fetch_and_add (&s, -1);
}

void
testc2 (void)
{
  (void) __sync_val_compare_and_swap (&c, -1, -3);
}
