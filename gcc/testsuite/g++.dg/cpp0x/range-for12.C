// Test for range-based for loop with templates
// and begin/end as member functions

// { dg-do run }
// { dg-options "-std=c++11" }

/* Preliminary declarations */
namespace pre
{
  struct iterator
  {
    int x;
    explicit iterator (int v) :x(v) {}
    iterator &operator ++() { ++x; return *this; }
    int operator *() { return x; }
    bool operator != (const iterator &o) { return x != o.x; }
  };

  struct container
  {
    int min, max;
    container(int a, int b) :min(a), max(b) {}
    iterator begin() const
    {
        return iterator(min);
    }
    iterator end() const
    {
        return iterator(max);
    }

  };

} //namespace pre

using pre::container;
extern "C" void abort(void);

container run_me_just_once()
{
    static bool run = false;
    if (run)
        abort();
    run = true;
    return container(1,2);
}

/* Template with dependent expression. */
template<typename T> int test1(const T &r)
{
  int t = 0;
  for (int i : r)
    t += i;
  return t;
}

/* Template with non-dependent expression and dependent declaration. */
template<typename T> int test2(const container &r)
{
  int t = 0;
  for (T i : r)
    t += i;
  return t;
}

/* Template with non-dependent expression (array) and dependent declaration. */
template<typename T> int test2(const int (&r)[4])
{
  int t = 0;
  for (T i : r)
    t += i;
  return t;
}

/* Template with non-dependent expression and auto declaration. */
template<typename T> int test3(const container &r)
{
  int t = 0;
  for (auto i : r)
    t += i;
  return t;
}

/* Template with non-dependent expression (array) and auto declaration. */
template<typename T> int test3(const int (&r)[4])
{
  int t = 0;
  for (auto i : r)
    t += i;
  return t;
}

int main ()
{
  container c(1,5);
  int a[4] = {5,6,7,8};

  for (auto x : run_me_just_once())
      ;

  if (test1 (c) != 10)
    abort();
  if (test1 (a) != 26)
    abort();

  if (test2<int> (c) != 10)
    abort();
  if (test2<int> (a) != 26)
    abort();

  if (test3<int> (c) != 10)
    abort();
  if (test3<int> (a) != 26)
    abort();
  return 0;
}
