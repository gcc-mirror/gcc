// { dg-do assemble  }

template <class T>
struct vector {};

template<class T>
void fn(T)
{
  enum tern { H, L, X, U };

  vector<tern> ternvec;
}

template void fn(int);
