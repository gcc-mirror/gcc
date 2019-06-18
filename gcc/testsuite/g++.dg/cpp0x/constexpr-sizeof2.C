// PR c++/90825 - endless recursion when evaluating sizeof.
// { dg-do compile { target c++11 } }

class address {
  char host_[63];
public:
  static constexpr unsigned buffer_size() noexcept { return sizeof(host_); }
};

template <class Archive>
void load()
{
  char host[address::buffer_size()];
}
