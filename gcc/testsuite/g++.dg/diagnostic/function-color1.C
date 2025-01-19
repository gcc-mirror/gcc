// Verify colorization of printing of function declarations.
// Use dg-*-multiline-output to avoid regexp interpretation.

// { dg-options "-fdiagnostics-color=always" }

template <class T> void f(short t);
template <class T> void f(long t);

int main()
{
  f<int>(42);
  /* { dg-begin-multiline-output "" }
call of overloaded '[01m[Kf<int>(int)[m[K' is ambiguous
     { dg-end-multiline-output "" } */
  /* { dg-begin-multiline-output "" }
candidate 1: '[01m[Kvoid[01;32m[K f[m[K(short int) [35m[K[with T = int][m[K[m[K'
     { dg-end-multiline-output "" } */
}

// Discard the remaining colorized output that confuses dejagnu.
// { dg-prune-output diagnostic/function-color1.C }
