// Build don't link:

template <class T>
struct vector {};

template<class T>
void fn(T)
{
  enum tern { H, L, X, U };

  vector<tern> ternvec; // ERROR - composed from a local type
}

template void fn(int);
