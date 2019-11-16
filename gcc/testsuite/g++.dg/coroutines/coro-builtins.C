//  { dg-additional-options "-fsyntax-only " }

typedef __SIZE_TYPE__ size_t;

int main ()
{
  void *co_h;
  void *promise;
  const size_t co_align = 16;

  bool d = __builtin_coro_done (co_h);
  __builtin_coro_resume (co_h);
  __builtin_coro_destroy (co_h);
  promise = __builtin_coro_promise (co_h, co_align, true);

  return 0;
}
