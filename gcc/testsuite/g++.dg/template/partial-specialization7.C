// PR c++/81102

template <typename FuncSig, FuncSig f>
struct HelperWrapper;

// [...]

template <typename Ret, Ret (&Func)()>
struct HelperWrapper<Ret (&)(), Func>
{
    static inline int WrapFuncT(const int)
    {
        return 0; // Changed
    }
};

// Unary
template <typename Ret, typename Arg1, Ret (&Func)(Arg1)>
struct HelperWrapper<Ret (&)(Arg1), Func>
{
    static inline int WrapFuncT(const int)
    {
        return 1; // Changed
    }
};

// Binary
template <typename Ret, typename Arg1, typename Arg2, Ret (&Func)(Arg1, Arg2)>
struct HelperWrapper<Ret (&)(Arg1, Arg2), Func>
{
    static inline int WrapFuncT(const int)
    {
        return 2; // Changed
    }
};

int main()
{
  return 0;
}
