/* PR target/39423 */

int
foo (const char *name, int nmlen, char *flags)
{
  const char *nonspc;
  int len, n, lfn;
  int needlfn[2], dotspc[2];
  unsigned char locale[2];
  for (nonspc = &name[nmlen - 1]; nonspc >= name && *nonspc == ' '; ++n)
    {
      if (!nmlen)
	{
	  needlfn[name >= nonspc] = !0, dotspc[n != 0] =
	    locale[0], --n, name += len, nmlen -= len;
	}
    }
  if (!lfn && ((dotspc[0] == ' ' && !(len & 0x0010)) || dotspc[0] == '.'))
    return 22;
  if (!(needlfn[0] || needlfn[1]))
    *flags |= 0x02;
}
