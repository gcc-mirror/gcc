/* { dg-do run } */
/* { dg-require-effective-target int32plus } */

struct f {
  unsigned au : 5;
  unsigned f3 : 21;
} g_994;

int main()
{
  struct f aq1 = {};
    {
      struct f aq = {9, 5};
      struct f as = aq;
      for (int y = 0 ; y <= 4; y += 1)
	if (as.au)
	  {
	    struct f aa[5] = {{2, 154}, {2, 154}, {2, 154}, {2, 154}, {2, 154}};
	    as = aa[0];
	  }
      aq1 = as;
    }
  if (aq1.f3 != 0x9a)
    __builtin_abort();
  return 0;
}
