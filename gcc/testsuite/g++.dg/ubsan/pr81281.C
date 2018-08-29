// PR sanitizer/81281
// { dg-do run }
// { dg-options "-fsanitize=undefined -fno-sanitize-recover=undefined" }
// { dg-additional-sources "pr81281-aux.cc" }

extern const int ci;
extern int i;
extern long long ll;

int
foo ()
{
  int a = (int) (-2024172551 - i - (ci - ll))
	  - ((int) (-2024172551 - i - (ci - ll))
	     - (int) (-2024172551 - (long long)ci));
  return a;
}

int
main ()
{
  if (__SIZEOF_INT__ * __CHAR_BIT__ == 32
      && __SIZEOF_LONG_LONG__ * __CHAR_BIT__ == 64)
    foo ();
  return 0;
}
