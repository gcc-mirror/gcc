// PR 83988 ICE

template<class T> struct optional {};
struct get_from_json {
  template<typename GetWhat>
  operator optional<GetWhat>() const {return optional<GetWhat> ();}
  template<typename AsWhat>
  optional<AsWhat> maybe() const
  {
    return this->operator optional<AsWhat>();
  }
};
void test()
{
  get_from_json().maybe<int>();
}
