// { dg-do assemble  }

template <bool B>
struct S
{
  static void g();
};

template <bool B>
void g();

template<unsigned Length>
void f()
{
  const bool b = true;
  g<b>();
  const bool b1 = (Length == 2);
  S<b1>::g();
}

void h()
{
  f<3>();
}
