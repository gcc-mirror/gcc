// Check that ARM vector delete functions accept NULL pointers as
// inputs.
// { dg-do run { target arm*-*-* } }

#ifdef __ARM_EABI__
#include <cxxabi.h>

typedef void *(dtor_type)(void *);

extern "C" {
  void abort();
  void *__aeabi_vec_dtor_cookie(void *, dtor_type);
  void __aeabi_vec_delete(void *, dtor_type);
  void __aeabi_vec_delete3(void *, 
			   dtor_type, 
			   void (*)(void *, __SIZE_TYPE__));
  void __aeabi_vec_delete3_nodtor(void *, 
				  void (*)(void *, __SIZE_TYPE__));
}

// These functions should never be called.
void* dtor(void *)
{
  abort ();
}

void dealloc(void *, size_t) {
  abort ();
}

int main () {
  if (__aeabi_vec_dtor_cookie (NULL, &dtor) != NULL)
    return 1;
  // These do not return values, but should not crash.
  __aeabi_vec_delete (NULL, &dtor);
  __aeabi_vec_delete3 (NULL, &dtor, &dealloc);
  __aeabi_vec_delete3_nodtor (NULL, &dealloc);
}
#else
int main () {}
#endif
