// PR c++/48909
// { dg-options -std=c++11 }

#define SA(X) static_assert((X),#X)

constexpr int const * is_sorted_until(int const * first, int const * last)
{
 return first == last || first + 1 == last ? last
  : (*(first + 1) < *first) != false ? first + 1
  : is_sorted_until(first + 1, last);
}

int main()
{
 static constexpr int array[2] = {0, 1};
 constexpr int const * last = is_sorted_until(array, array + 2);
 SA(last==array+2);
}
