// Test for subsequence checking in overload resolution.

class foo {
public:
  void operator <<(char *) { }
  void operator <<(const char * const &);
};
 
int
main()
{
  char s[20];
  foo f;
  f << s;
}
