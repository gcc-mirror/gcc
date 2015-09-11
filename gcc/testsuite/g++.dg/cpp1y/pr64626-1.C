// PR c++/64626
// { dg-do compile { target c++14 } }

#define STR(s) #s
int
main()
{
  int i = 1'2;
  const char *s[3]
  {
    STR(1' '),
    STR(..),
    STR(%:%),
  };
}
#if 0
1' '
..
%:%
#endif
