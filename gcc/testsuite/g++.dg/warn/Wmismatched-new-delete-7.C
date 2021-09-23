/* PR middle-end/101791 - missing warning on a mismatch between scalar
   and array forms of new and delete
   Verify that likely safe calls to technically mismatched member operator
   new and delete are not diagnosed.  This test might need to be adjusted
   if it turns out the assumptions GCC makes are overly conservative.
   { dg-do compile }
   { dg-options "-Wall" } */

typedef __SIZE_TYPE__ size_t;

namespace std
{
#if __cplusplus >= 201703L
enum class align_val_t: size_t { };
#else
enum align_val_t { };
#endif

struct nothrow_t { };
const nothrow_t nothrow = { };

}

void sink (void*, ...);

struct X { } x;
struct Y { } y;

struct A
{
  void* operator new (size_t);
  void* operator new (size_t, std::align_val_t);

  void* operator new (size_t, X);
  void* operator new (size_t, Y);

  void* operator new (size_t, std::align_val_t, X);
  void* operator new (size_t, std::nothrow_t, Y);

  /* A single operator delete callable on the result of calls to any
     of the operator new overloads above (this may be too optimistic).  */
  void operator delete (void*);
};

A* nowarn_align ()
{
  /* The following are likely okay given A's definition above but would
     not be if A also defined an align_val_t overload of operator delete.  */
  A *p = new (std::align_val_t (8)) A;
  delete p;

  return new (std::align_val_t (16)) A;
}

A* nowarn_X ()
{
  /* The following are also likely okay given A's definition above but
     also would not be if A also defined an overload of operator delete
     for X.  */
  A *p = new (x) A;
  delete p;
  return new (x) A;
}

A* nowarn_Y ()
{
  // Same as above.
  A *p = new (y) A;
  delete p;
  return new (y) A;
}


A* nowarn_align_X ()
{
  // Same as above.
  A *p = new (std::align_val_t (32), x) A;
  delete p;

  return new (std::align_val_t (64), x) A;
}


A* nowarn_nothrow_Y ()
{
  // Same as above.
  A *p = new (std::nothrow, y) A;
  delete p;
  return new (std::nothrow, y) A;
}

