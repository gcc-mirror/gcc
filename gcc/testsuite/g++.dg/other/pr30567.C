/* { dg-do run } */
/* { dg-options "-O -finline-functions -fstrict-aliasing" } */

template <typename T>
struct const_ref
{
  const T* begin;
  const_ref(const T* b) : begin(b) {}
};

template <typename T>
T sum(const_ref<T> const& a)
{
  T result = 0;
  for(unsigned i=0;i<1;i++) result += a.begin[i];
  return result;
}

struct tiny_plain
{
  int elems[2];
  tiny_plain() { elems[0]=1; }
};

struct vec3 : tiny_plain {};

struct mat3
{
  int type() const { return sum(const_ref<int>(vec3().elems)) == 1; }
};

int main() { return mat3().type() ? 0 : 1; }

