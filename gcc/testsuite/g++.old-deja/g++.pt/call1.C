// Build don't link:

struct IsCompressed { };
struct Field {
  bool IsCompressed() const { return true; }
};

template<class C>
inline bool
for_each(const Field& p, IsCompressed, C)
{
  return p.IsCompressed();
}
