// PR c++/38064
// { dg-do run { target c++11 } }

enum class E { elem };

template <class T>
void f (T t);

bool f (bool b) { return b; }

int main()
{
  E e = E::elem;
  if (!f (e == E::elem))
    return 1;
  if (!f (e <= E::elem))
    return 1;
  if (!f (e >= E::elem))
    return 1;
  if (f (e < E::elem))
    return 1;
  if (f (e > E::elem))
    return 1;
  if (f (e != E::elem))
    return 1;
}
