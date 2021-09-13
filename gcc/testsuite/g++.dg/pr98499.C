/* PR tree-optimization/98499.  */
/* { dg-do run } */
/* { dg-options "-O2" } */

struct string {
  // pointer to local store
  char * _M_buf;
  // local store
  char _M_local_buf[16];

  __attribute__((noinline)) string() : _M_buf(_M_local_buf) {}

  ~string() {
    if (_M_buf != _M_local_buf)
      __builtin_trap();
  }

  string(const string &__str); // no copies
};

__attribute__((noinline)) static string dir_name() { return string(); }
class Importer {
  string base_path;

public:
  __attribute__((noinline)) Importer() : base_path (dir_name()) {}
};

int main() {
  Importer imp;
}
