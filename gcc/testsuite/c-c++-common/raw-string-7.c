// The trailing whitespace after \ and before newline extension
// breaks full compliance for raw strings.
// { dg-do run { xfail *-*-* } }
// { dg-options "-std=gnu99" { target c } }
// { dg-options "-std=c++0x" { target c++ } }

// Note, there is a single space after \ on the following line.
const char *s0 = R"(\ 
)";
// { dg-bogus "backslash and newline separated by space" "" { xfail *-*-* } 8 }

// Note, there is a single tab after \ on the following line.
const char *s1 = R"(\	
)";
// { dg-bogus "backslash and newline separated by space" "" { xfail *-*-* } 13 }

int
main (void)
{
  if (__builtin_strcmp (s0, "\\ \n") != 0
      || __builtin_strcmp (s1, "\\\t\n") != 0)
    __builtin_abort ();
  return 0;
}
