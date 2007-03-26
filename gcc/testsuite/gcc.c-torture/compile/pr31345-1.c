/* PR tree-optimization/31345
   This caused a crash in VRP when dealing with overflow infinities.  */

void
dpsnaffle (const char *kbuf)
{
  int hash, thash, head[2], off;
    {
      int _DP_i;
      (hash) = 19780211;
        {
          (hash) = (hash) + (kbuf)[_DP_i];
        }
      (hash) = ((hash) * 43321879) & 0x7FFFFFFF;
    }
  while (off != 0)
    {
      if (hash > thash) {}
      else if (hash < thash)
        {
          off = head[2];
        }
    }
}
