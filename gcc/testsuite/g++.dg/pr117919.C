/* PR tree-optimization/117919 */
/* { dg-do compile } */
/* { dg-options "-O1 -fno-tree-forwprop -fnon-call-exceptions --param=early-inlining-insns=192 -std=c++20" } */

char _M_p, _M_construct___beg;
struct _Alloc_hider {
  _Alloc_hider(char);
};
long _M_string_length;
void _M_destroy();
void _S_copy_chars(char *, char *, char *) noexcept;
char _M_local_data();
struct Trans_NS___cxx11_basic_string {
  _Alloc_hider _M_dataplus;
  bool _M_is_local() {
    if (_M_local_data())
      if (_M_string_length)
        return true;
    return false;
  }
  void _M_dispose() {
    if (!_M_is_local())
      _M_destroy();
  }
  char *_M_construct___end;
  Trans_NS___cxx11_basic_string(Trans_NS___cxx11_basic_string &)
      : _M_dataplus(0) {
    struct _Guard {
      ~_Guard() { _M_guarded->_M_dispose(); }
      Trans_NS___cxx11_basic_string *_M_guarded;
    } __guard0;
    _S_copy_chars(&_M_p, &_M_construct___beg, _M_construct___end);
  }
};
namespace filesystem {
struct path {
  path();
  Trans_NS___cxx11_basic_string _M_pathname;
};
} // namespace filesystem
struct FileWriter {
  filesystem::path path;
  FileWriter() : path(path) {}
};
struct LanguageFileWriter : FileWriter {
  LanguageFileWriter(filesystem::path) {}
};
int
main() {
  filesystem::path output_file;
  LanguageFileWriter writer(output_file);
}
