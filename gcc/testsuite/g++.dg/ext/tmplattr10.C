// PR c++/95222
// { dg-do compile { target { { { i?86-*-* x86_64-*-* } && ia32 } && c++11 } } }

#if defined(_MSC_VER)
#define CC_FASTCALL __fastcall
#define CC_STDCALL __stdcall
#else
#define CC_FASTCALL __attribute__((fastcall))
#define CC_STDCALL __attribute__((stdcall))
#endif

template <typename FuncT>
struct FuncResult;

template <typename R, typename... Args>
struct FuncResult<R(*)(Args...)>
{
    using type = R;
};

template <typename R, typename... Args>
struct FuncResult<R(CC_FASTCALL*)(Args...)>
{
    using type = R;
};

template <typename R, typename... Args>
struct FuncResult<R(CC_STDCALL*)(Args...)>
{
    using type = R;
};

template <typename FuncT>
auto wrap(FuncT f) -> typename FuncResult<FuncT>::type
{
    return f(1, 2, 3);
}

int CC_FASTCALL func1(int x, int y, int z)
{
    return x + y + z;
}

int CC_STDCALL func2(int x, int y, int z)
{
    return x + y + z;
}

int main()
{
    return wrap(&func1) + wrap(&func2);
}
