/* PR optimization/13313 */
/* Origin: Mike Lerwill <mike@ml-solutions.co.uk> */

extern void abort(void);

void DisplayNumber (unsigned long v)
{
  if (v != 0x9aL)
    abort();
}

unsigned long ReadNumber (void)
{
  return 0x009a0000L;
}

int main (void)
{
  unsigned long tmp;
  tmp = (ReadNumber() & 0x00ff0000L) >> 16;
  DisplayNumber (tmp);
  return 0;
}
