// PR c++/90393
// { dg-do run }

int c, d;

struct string {
  string(const char *p): s(p) { ++c; }
  ~string() { ++d; }
  string(const string& str): s(str.s) { ++c; }
  const char* s;
  bool empty() const { return !s; }
};
    
string foo()
{
  string s("foo");
  return s.empty() ? throw "empty" : s;
}

int main()
{
  foo();
  if (c != d)
    __builtin_abort();
}
