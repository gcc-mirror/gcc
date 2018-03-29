// Test for errors in range-based for loops
// with member begin/end

// { dg-do compile { target c++11 } }

struct container1
{
    int *begin();
    //no end
};

struct container2
{
    int *end();
    //no begin
};

struct container3
{
private:
    int *begin(); // { dg-message "private" }
    int *end(); // { dg-message "private" }
};

struct container4
{
    int *begin;
    int *end;
};

struct container5
{
    typedef int *begin;
    typedef int *end;
};

struct callable
{
    int *operator()();
};

struct container6
{
    callable begin;
    callable end;
};

struct container7
{
    static callable begin;
    static callable end;
};

struct container8
{
    static int *begin();
    int *end();
};

struct private_callable
{
private:
    int *operator()(); // { dg-message "private" }
};

struct container9
{
    private_callable begin;
    private_callable end;
};

struct container10
{
    typedef int *(*function)();

    function begin;
    static function end;
};

namespace N
{
template<typename T> int *begin(T &t)
{
    return 0;
}
template<typename T> int *end(T &t)
{
    return 0;
}
struct container11
{
    int *begin();
    //no end
};

struct container12
{
    int *end();
    //no begin
};

struct container13
{
};
}

void test1()
{
  for (int x : container1()); // { dg-error "'begin' was not declared|'end' was not declared" }
  for (int x : container2()); // { dg-error "'begin' was not declared|'end' was not declared" }
  for (int x : container3()); // { dg-error "within this context" }
  for (int x : container4()); // { dg-error "cannot be used as a function" }
  for (int x : container5()); // { dg-error "invalid use of" }
  for (int x : container6());
  for (int x : container7());
  for (int x : container8());
  for (int x : container9()); // { dg-error "within this context" }
  for (int x : container10());
  for (int x : N::container11());
  for (int x : N::container12());
  for (int x : N::container13());
}
