
template <typename T> auto frob (T t)
{
  return t;
}

struct Bob 
{
  operator auto () 
  {
    return 0;
  }
};

inline auto foo ()
{
  return frob (1) + int (Bob ());
}

