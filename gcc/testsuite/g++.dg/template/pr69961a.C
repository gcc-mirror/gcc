// PR c++/69961
// { dg-do compile { target c++11 } }
// { dg-skip-if "requires hosted libstdc++ for string" { ! hostedlib } }

#include <string>

using std::string;

class Format {
 public:
  explicit Format(string formatted) {}
  string buffer;
};

string StrCat(const string& a) {
  return "";
}

template <typename... AV>
Format Message(string msg, const AV&... args) {
  return Format::Format(StrCat(msg, args...)); // { dg-error "cannot call constructor" }
}

int main(int, char**) {
  Message("msg");
}
