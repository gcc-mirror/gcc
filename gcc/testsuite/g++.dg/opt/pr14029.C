// { dg-do run }
// { dg-options "-O2" }
// We used to mis-compile this testcase as we did not know that
// &a+offsetof(b,a) was the same as &a.b

struct Iterator {
    int * ptr;

    Iterator(int * i) : ptr(i) { }
    void operator++() { ++ptr; }
    int *const & base() const { return ptr; }
};


Iterator find_7(Iterator first, Iterator last)
{
  int trip_count  = (last.base() - first.base()) >> 1;

  for ( ; trip_count > 0 ; --trip_count) {
    if (*first.ptr == 7) return first;
    ++first;

    if (*first.ptr == 7) return first;
    ++first;
  }

  switch(last.base() - first.base()) {
    case 1:
          if (*first.ptr == 7) return first;
          ++first;
    case 0:
    default:
          return last;
  }
}

int main() {
  int as[5] = {4,4,4,4,7};
  return (find_7(Iterator(&as[0]), Iterator(&as[5])).ptr == &as[5]);
};

