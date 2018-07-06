// PR c++/84231 - overload resolution with cond_expr in a template

// { dg-do compile }

struct format {
  template<typename T> format& operator%(const T&) { return *this; }
  template<typename T> format& operator%(T&) { return *this; }
};

format f;

template <typename>
void function_template(bool b)
{
  // Compiles OK with array lvalue:
  f % (b ? "x" : "x");

  // Used to fails with pointer rvalue:
  f % (b ? "" : "x");
}

void normal_function(bool b)
{
  // Both cases compile OK in non-template function:
  f % (b ? "x" : "x");
  f % (b ? "" : "x");

  function_template<void>(b);
}
