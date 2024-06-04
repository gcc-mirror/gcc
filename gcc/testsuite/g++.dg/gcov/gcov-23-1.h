#include <string>
struct fst {
  static fst& instance ()
  {
    static fst self;
    return self;
  }
  std::string data;
};
