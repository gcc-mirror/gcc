// { dg-do compile }

void
large_array_char(int n)
{
  new char[n]
    [1ULL << (sizeof(void *) * 4)]
    [1ULL << (sizeof(void *) * 4)]; // { dg-error "size of array" }
}

template <typename T>
void
large_array_char_template(int n)
{
  new char[n]
    [1ULL << (sizeof(void *) * 4)]
    [1ULL << (sizeof(void *) * 4)]; // { dg-error "size of array" }
}


template <typename T>
void
large_array_template1(int n)
{
  new T[n] // { dg-error "size of array exceeds maximum object size" }
    [(1ULL << (sizeof(void *) * 4)) / sizeof(T)]
    [1ULL << (sizeof(void *) * 4)];
}

template <typename T>
void
large_array_template2(int n)
{
  new T[n] // { dg-error "size of array exceeds maximum object size" }
    [(1ULL << (sizeof(void *) * 4)) / sizeof(T)]
    [1ULL << (sizeof(void *) * 4)];
}

template <typename T>
void
large_array_template3(int n)
{
  new T[n] // { dg-error "size.*of array exceeds maximum object size" }
    [(1ULL << (sizeof(void *) * 4)) / sizeof(T)]
    [1ULL << (sizeof(void *) * 4)];
}

void
call_large_array_template(int n)
{
  large_array_template1<char>(n);
  large_array_template2<int>(n);
  large_array_template3<double>(n);
}
