// PR c++/49136
// { dg-do compile }
// { dg-options "-std=c++11" }

struct day
{
  unsigned d : 5;
  unsigned n : 3;
  constexpr explicit day (int dd) : d(dd), n(7) {}
};

struct date {
  int d;
  constexpr date (day dd) : d(dd.n != 7 ? 7 : dd.d) {}
};

constexpr day d(0);
constexpr date dt(d);
static_assert (dt.d == 0, "Error");
