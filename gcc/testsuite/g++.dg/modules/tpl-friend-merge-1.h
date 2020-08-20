
template<typename T>
class basic_streambuf
{
  friend void __istream_extract (int);

  // something private
  static constexpr int field = 5;
};
