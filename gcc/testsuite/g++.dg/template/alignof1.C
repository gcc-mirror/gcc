template<typename T>
int my_alignof()
{
    return __alignof__(T);
}

template<typename>
struct X { };

int main()
{
  return my_alignof<X<void> >();
}
