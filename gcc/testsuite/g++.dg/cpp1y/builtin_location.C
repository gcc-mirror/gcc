// PR c++/66561 - __builtin_LINE at al. should yield constant expressions
// { dg-do compile { target c++11 } }
#define A(expr) static_assert ((expr), #expr)

#define FILE_1 "file_name.suffix"
#define FILE_2 "some_other_file_name.suffix"

#line 1 FILE_1
constexpr const char*
file1 ()
{
#if __cplusplus >= 201402L
  // Do extra checking in C++ 14 and later.
  constexpr const char *f1 = __FILE__;
  constexpr const char *f2 = __builtin_FILE ();
  A (0 == __builtin_strcmp (f1, f2));
  return f1;
#else
  // In C++ 11, a constexpr function body must consist of a single
  // return statement and no declaratations.
  return __builtin_FILE ();
#endif  
}

#line 1 FILE_2
constexpr const char*
file2 ()
{
#if __cplusplus >= 201402L
  constexpr const char *f1 = __FILE__;
  constexpr const char *f2 = __builtin_FILE ();
  A (0 == __builtin_strcmp (f1, f2));
  return f1;
#else
  return __builtin_FILE ();
#endif
}

#line 1 "bogus file name"
constexpr const char*
this_file (const char *fname = __builtin_FILE ())
{
  return fname;
}

constexpr const char*
function ()
{
#if __cplusplus >= 201402L
  constexpr const char *f1 = __FUNCTION__;
  constexpr const char *f2 = __builtin_FUNCTION ();
  A (0 == __builtin_strcmp (f1, f2));
  return f1;
#else
  return __builtin_FUNCTION ();
#endif
}

constexpr const char*
this_function (const char *func = __builtin_FUNCTION ())
{
  return func;
}

constexpr int
line ()
{
#if __cplusplus >= 201402L
#line 123
  constexpr int n1 = __LINE__;
  constexpr int n2 = __builtin_LINE ();
  A (123 == n1);
  A (n1 + 1 == n2);
  return n2;
#else
#line 123
  // Newline.
  return __builtin_LINE ();
#endif
}

constexpr int
this_line (int line = __builtin_LINE ())
{
  return line;
}


// Exercise __builtin_FILE().
#line 1 "foobar"
constexpr const char* f1 = file1 ();
A (0 == __builtin_strcmp (f1, FILE_1));

#line 2 "foobar"
constexpr const char* f2 = file2 ();
A (0 == __builtin_strcmp (f2, FILE_2));

#define FILE_3 "this_file_name_right_here.this_suffix"
#line 1 FILE_3
constexpr const char* f3 = this_file ();
A (0 == __builtin_strcmp (f3, FILE_3));

#define FILE_4 "next_file_name.another_suffix"
#line 1 "foobar"
constexpr const char* f4 = this_file
#line 1 FILE_4
  (
#line 1 "foobar"
   )
  ;
A (0 == __builtin_strcmp (f4, FILE_4));


// Exercise __builtin_FUNCTION().

// Verify that __builtin_FUNCTION() returns the name of the function
// in which it is called.
constexpr const char* fun1 = function ();
A (0 == __builtin_strcmp (fun1, "function"));

// Verify that __builtin_FUNCTION() returns the empty string when
// it's invoked to set the default argument value in a function
// called at file scope.
constexpr const char* fun2 = this_function ();
A (0 == __builtin_strcmp (fun2, ""));

constexpr const char*
named_function ()
{
  return this_function ();
}

constexpr const char* fun3 = named_function ();
A (0 == __builtin_strcmp (fun3, "named_function"));


// Exercise __builtin_LINE().
// Verify the line numbe returned by the built-in.
#line 4
constexpr int n1 = __builtin_LINE ();
A (n1 == 4);

// Verify the line number obtained by a constexpr function.
#line 5
constexpr int n2 = line ();
A (n2 == 124);

// Verify the line number determined by the default argument.
#line 6
constexpr int n3 = this_line ();
A (n3 == 6);

// Verify that the line number accounts for each of the calls.
#line 7
constexpr int n4 = this_line () + this_line ();
A (n4 == 14);

// Verify that the line number accounts for each of the calls when
// split over multiple lines.
#line 1
constexpr int n5 = this_line ()
#line 8
  + this_line ();
A (n5 == 9);

// Verify that the line number corresponds to the closing parenthesis
// of the function call.
#line 1
constexpr int n6 = this_line
#line 99
  (
#line 1
   )
  ;
A (n6 == 99);
