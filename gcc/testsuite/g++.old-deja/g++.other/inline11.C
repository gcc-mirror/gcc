// Build don't link:
// Origin: Jakub Jelinek <jakub@redhat.com>
// Special g++ Options: -O2

class baz
{
public:
  baz& operator += (const baz&);
};
 
inline baz& baz::operator += (const baz& r)
{
  return *this;
}

inline baz operator + (int x, const baz& y)
{
  return y;
}

static inline baz bar (int alpha);
static inline baz foo (int alpha)
{
  baz tmp = alpha + foo (alpha);
  tmp += alpha + bar (alpha);
  return tmp;
}

static inline baz bar (int alpha)
{
  baz tmp = alpha + bar (alpha);
  tmp += alpha + foo (alpha);
  return tmp;
}
