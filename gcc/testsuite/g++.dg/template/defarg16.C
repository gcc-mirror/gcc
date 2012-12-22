// PR c++/55127

struct some_class
{
  static const bool     is_valid_type = true;
};

template< typename Type
        , bool Valid = Type::is_valid_type
>
struct wrapper;

template< typename Type >
struct wrapper< Type, true >
{
  typedef Type type;
};

template< typename T >
void fun()
{
  wrapper<some_class>::type x;
}

int main()
{
  fun<int>();
}
