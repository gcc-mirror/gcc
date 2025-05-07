// { dg-do compile }
// { dg-options "-O -fipa-pta" }

struct basic_ios {
  ~basic_ios();
};
struct basic_istream : virtual basic_ios {};
template <typename> struct basic_ifstream : basic_istream {
  template <typename _Path> basic_ifstream(_Path, int);
};
extern template class basic_ifstream<char>;
void CompareFiles_path2() { basic_ifstream<char>(CompareFiles_path2, 0); }
