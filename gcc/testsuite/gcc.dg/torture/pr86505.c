/* { dg-do run } */

static inline __attribute__(( __always_inline__)) int 
funA(unsigned int param, ...) 
{ 
  return __builtin_va_arg_pack_len(); 
}

static inline __attribute__(( __always_inline__)) int
funB(unsigned int param, ...)
{ 
  return funA(param,  2, 4, __builtin_va_arg_pack()); 
}

int 
testBuiltin(void) 
{ 
  int rc = funB(0,1,2); 
  if (rc != 4)
    return 1;
  return 0;
}

int
main()
{
  int rc = testBuiltin();
  if (rc == 1)
    __builtin_abort ();

  return 0;
}
