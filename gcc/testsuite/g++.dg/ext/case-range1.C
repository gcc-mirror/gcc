// { dg-do compile }
// Tests if case ranges (a GNU extension) are accepted
// { dg-options "" }
const int low = -2;
const int high = 15;

template <typename T>
T f2 (T i)
{
  switch (i)
  {
    case low ... high : return i + 1;
    default : return 0;
  }
}

int f (int i)
{
  switch (i) {
    case 1 ... 10: return i + 1;
    default: return f2 (i);
  }
}
