// { dg-do run }

typedef unsigned uint32_t;

template<typename T>
struct AlignedStorage2
{
  char mBytes[sizeof(T)];

  const T* addr() const { return reinterpret_cast<const T*>(mBytes); }
  T* addr() { return reinterpret_cast<T*>(mBytes); }
};

struct Register {
    uint32_t reg_;
};

class TypedOrValueRegister
{
  AlignedStorage2<Register> typed;
  __attribute__((noinline)) Register& dataTyped() { return *typed.addr(); }
public:
  TypedOrValueRegister(Register reg)
    {
      dataTyped() = reg;
    }
  Register typedReg() const { return *typed.addr(); }
};

int main() {
    Register reg = { 10u };
    if (TypedOrValueRegister(reg).typedReg().reg_ != 10)
      __builtin_abort();
    return 0;
}
