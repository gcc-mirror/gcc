// { dg-do compile }
// Tests if case ranges (a GNU extension) emit errors in ISO mode
// { dg-options "-pedantic" }
const int low = -2;
const int high = 15;

template <typename T>
T f2 (T i)
{
  switch (i)
  {
    case low ... high : return i + 1; // { dg-warning "non-standard" }
    default : return 0;
  }
}

int f (int i)
{
  switch (i) {
    case 1 ... 10: return i + 1;      // { dg-warning "non-standard" }
    default: return f2 (i);           // { dg-message "required" }
  }
}
