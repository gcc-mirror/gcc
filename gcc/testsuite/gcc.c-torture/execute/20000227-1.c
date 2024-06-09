void abort(void);

static const unsigned char f[] = "\0\377";
static const unsigned char g[] = "\0ÿ";

int main(void)
{
  if (sizeof f != 3 || sizeof g != 3)
    abort ();
  if (f[0] != g[0])
    abort ();
  if (f[1] != g[1])
    abort ();
  if (f[2] != g[2])
    abort ();
  return 0;
}
