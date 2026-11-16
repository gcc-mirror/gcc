/* PR ipa/128602 */
/* { dg-do compile } */
/* { dg-options "-O3 -fdump-ipa-cp-details" } */

int counter;

static void
kernel (int k)
{
  for (int i = 0; i < k; i++)
    counter += i;
}

__attribute__ ((noinline, callback_only (1, 2))) static void
dispatch (void (*fn) (int), int k)
{
  fn (k);
}

void
run (void)
{
  dispatch (kernel, 3);
}

/* { dg-final { scan-ipa-dump "no longer has any callback attributes" cp } } */
