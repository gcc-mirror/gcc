// Build don't link:

struct IsCompressed { };
struct Field {
};

template<class C>
inline bool
for_each(const Field& p, IsCompressed, C)
{
  return p.IsCompressed(); // ERROR - calling type like a method
}

template bool for_each<int>(const Field& p, IsCompressed, int);
