// DR 1709 - Stringizing raw string literals containing newline
// { dg-do run { target c++26 } }

#define A(a) #a
const char *a = A(a\f\\b"c");
const char *b = A(R"abc(a\b

)abc");

int
main ()
{
  if (a[1] != '\f' || a[2] != '\\' || a[4] != '"' || a[6] != '"')
    __builtin_abort ();
  if (b[1] != '"' || b[7] != '\\' || b[9] != '\n' || b[10] != '\n'
      || b[11] != ')' || b[15] != '"')
    __builtin_abort ();
}
