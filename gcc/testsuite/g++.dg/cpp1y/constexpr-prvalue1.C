// PR c++/116416
// { dg-do compile { target c++14 } }
// { dg-options "-O" }

struct Str {
  constexpr Str() {}
  constexpr Str(const char *instr) {
      str = instr; length = 0;
      for (auto index = 0; instr[index]; ++index) {
        ++length;
      }
  }
  const char *str = nullptr;
  int length = 0;
};
extern void callback(Str str);
void
func1()
{
    callback(Str{"Test"});
}
void
func2()
{
    Str str{"Test"};
    callback(str);
}

// Check that we don't call Str::Str(char const*)
// { dg-final { scan-assembler-not "_ZN3StrC1EPKc" } }
