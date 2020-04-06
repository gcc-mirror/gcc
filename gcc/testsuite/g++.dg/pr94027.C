// { dg-do compile { target c++11 } }
// PR 94027 ICE mangling

class a {
public:
  a (char);
};
struct b {
  b (a);
};
template <typename... aw, int...>
void ax (int)
{
  struct c : b {
    c () : b {sizeof...(aw)}
    {}
  };
}

void az() {
  ax ({});
}
