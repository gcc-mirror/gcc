// { dg-do run { target { { i?86-*-* x86_64-*-* } && ia32 } } }
// { dg-options "-Wabi=8 -save-temps" }
// { dg-final { scan-assembler "_Z18IndirectExternCallIPU7stdcallU7regparmILi3EEFviiEiEvT_T0_S3_" } }

typedef __SIZE_TYPE__ size_t;

template <typename F, typename T>
void IndirectExternCall(F f, T t1, T t2) { // { dg-warning "mangled name" }
  typedef F (*WrapF)(F);
  f (t1, t2);
}

__attribute__((regparm(3), stdcall))
void regparm_func (int i, int j)
{
  if (i != 24 || j != 42)
    __builtin_abort();
}

void normal_func (int i, int j)
{
  if (i != 24 || j != 42)
    __builtin_abort();
}

int main()
{
  IndirectExternCall (regparm_func, 24, 42);
  IndirectExternCall (normal_func, 24, 42);
}
