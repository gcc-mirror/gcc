/* { dg-do compile } */
/* { dg-options "-Ofast -fno-tree-dominator-opts" } */

struct locale_time_t
{
  const char *abday[7];
  const unsigned int *wabday[7];
};

static const unsigned int empty_wstr[1] = { 0 };

void
time_read (struct locale_time_t *time)
{
  int cnt;

  for (cnt=0; cnt < 7; cnt++)
    {
      time->abday[cnt] = "";
      time->wabday[cnt] = empty_wstr;
    }
}
