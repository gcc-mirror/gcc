
static int RawOrEnc = 0;
static inline void addpair(int fp, int un)
{
  if (RawOrEnc == 0 && fp != un)
    RawOrEnc = 1;
}
int f(int un0, char *a, unsigned int __s2_len)
{
  addpair(un0, un0);
  __s2_len < 4 ? __builtin_strcmp (a, "-") : 0;
}
