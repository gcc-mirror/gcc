typedef long time_t;
static __const int mon_lengths[2][12] = {
  31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,
  31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
};
static time_t
f (janfirst, year, rulep, offset)
     __const time_t janfirst;
     __const int year;
     register __const struct rule * __const rulep;
     __const long offset;
{
  register int leapyear;
  register time_t value;
  register int i;

  value += mon_lengths[leapyear][i] * ((long) (60 * 60) * 24);
}
