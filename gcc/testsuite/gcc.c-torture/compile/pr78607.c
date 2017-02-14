/* PR rtl-optimization/78607 */

void
rc (int cx)
{
  int mq;

  if (mq == 0 && (cx / 0) != 0)
    for (;;)
      {
      }
}
