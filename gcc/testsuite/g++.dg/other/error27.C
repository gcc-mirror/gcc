// PR c++/35332
// { dg-do compile }

void foo (double x, double y)
{
  __builtin_isgreater(x, y)();		// { dg-error "__builtin_\[^\n\]*cannot be used as a function" }
  __builtin_isless(x, y)();		// { dg-error "__builtin_\[^\n\]*cannot be used as a function" }
  __builtin_isgreaterequal(x, y)();	// { dg-error "__builtin_\[^\n\]*cannot be used as a function" }
  __builtin_islessequal(x, y)();	// { dg-error "__builtin_\[^\n\]*cannot be used as a function" }
  __builtin_isunordered(x, y)();	// { dg-error "__builtin_\[^\n\]*cannot be used as a function" }
  __builtin_islessgreater(x, y)();	// { dg-error "__builtin_\[^\n\]*cannot be used as a function" }
}
