// Test for other range-based for loops with
// begin/end member functions

// { dg-do compile { target c++11 } }

//These should not be used
template<typename T> int *begin(T &t)
{
    T::fail;
}
template<typename T> int *end(T &t)
{
    T::fail;
}

//Test for defaults

struct default1
{
    int *begin(int x); // { dg-message "note" }
    int *end();
};

struct default2
{
    int *begin(int x=0);
    int *end();
};

struct default3
{
    template <typename T> T *begin(); // { dg-message "note" }
    int *end();
};

struct default4
{
    template <typename T=int> T *begin();
    int *end();
};

struct default5
{
    template <typename T=int> T *begin(int x=0);
    int *end();
};

void test1()
{
  for (int x : default1()); // { dg-error "no matching function|note" }
  for (int x : default2());
  for (int x : default3()); // { dg-error "no matching function|note" }
  for (int x : default4());
  for (int x : default5());
}

//Inheritance tests

struct base_begin
{
    int *begin(); // { dg-message "" }
};

struct base_end
{
    int *end();
};

struct derived1 : base_begin, base_end
{
};

struct base_begin2 : base_begin
{
};

struct derived2 : base_begin, base_end, base_begin2 // { dg-warning "" }
{
};

struct base_begin3 : virtual base_begin
{
};

struct derived3 : virtual base_begin, base_end, base_begin3
{
};

void test2()
{
  for (int x : derived1());
  for (int x : derived2()); // { dg-error "is ambiguous" }
  for (int x : derived3());
}
