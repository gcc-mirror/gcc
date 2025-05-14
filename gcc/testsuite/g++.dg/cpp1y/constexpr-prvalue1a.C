// PR c++/116416
// A version of constexpr-prvalue1.C that calls __builtin_is_constant_evaluated.
// { dg-do compile { target c++14 } }
// { dg-options "-O -fdump-tree-original" }

struct Str {
  constexpr Str() {}
  constexpr Str(const char *instr) {
      str = instr; length = 0;
      for (auto index = 0; instr[index]; ++index) {
        length += __builtin_is_constant_evaluated() ? 1 : 1;
      }
  }
  const char *str = nullptr;
  int length = 0;
};
extern void callback(Str str);
void
func1()
{
    callback(Str{"Test1"});
}
void
func2()
{
    Str str{"Test2"};
    callback(str);
}

// Check that the front end folds both the temporary initializer and
// that of 'str'.
// { dg-final { scan-tree-dump "{.str=\\(const char \\*\\) \"Test1\", .length=5}" "original" } }
// { dg-final { scan-tree-dump "{.str=\\(const char \\*\\) \"Test2\", .length=5}" "original" } }
