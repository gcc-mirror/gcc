/* Origin: PR target/7434 Gwenole Beauchesne <gbeauchesne@mandrakesoft.com> */

int main(void)
{
  static const int align_g[] = { 1, 2, 4, 8, 16 };
  char * buf;
  int i = 0;
  volatile long double val = 0;
  val = *((long double *)(buf + align_g[i]));
  return 0;
}
