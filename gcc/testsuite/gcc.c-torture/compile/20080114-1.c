/* PR rtl-optimization/31944 */
/* Origin: Aurelien Jarno <aurelien@aurel32.net> */

int type;

void stuck(int res)
{
  if (type == 1) {
    if (res == 0) asm volatile("");
  }
  else if (type == 0) {
    if (res == 0) asm volatile("" : : "i" (0));
  }
}
