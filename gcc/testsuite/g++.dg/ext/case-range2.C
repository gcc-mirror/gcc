// { dg-do compile }
// Tests if case ranges (a GNU extension) correctly emit messages
// about overlapping ranges.
// { dg-options "" }

const int low = -2;
const int high = 15;

template <typename T>
T f2 (T i)
{
  switch (i)
  {
    case low ... high : return i + 1;  // { dg-error "previously" }
    case 5 : return i + 2;             // { dg-error "duplicate" }
    default : return 0;
  }
}

int f (int i)
{
  switch (i) {
    case 1 ... 10: return i + 1;       // { dg-error "first entry" }
    case 3 ... 5 : return i + 3;       // { dg-error "duplicate" }
    default: return f2 (i);            // { dg-message "required" }
  }
}
