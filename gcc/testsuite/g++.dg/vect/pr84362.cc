// { dg-do compile }
// { dg-require-effective-target c++11 }

constexpr unsigned int capacity = 1000;

struct vec
{
  int values[capacity];
  unsigned int _size = 0;
  unsigned int size() const noexcept { return _size; }
  void push(int x)
    {
      values[size()] = x;
      ++_size;
    }
};

int main()
{
  vec v;
  for(unsigned int i{0}; i != capacity; ++i)
    {
      v.push(i);
    }
  asm volatile("" : : "g"(&v) : "memory");
}

// { dg-final { scan-tree-dump "vectorized 1 loops in function" "vect" { target vect_int } } }
