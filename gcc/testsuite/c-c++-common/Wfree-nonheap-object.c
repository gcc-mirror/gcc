/* Verify that built-in forms of functions can be used interchangeably
   with their ordinary (library) forms in attribute malloc.
   { dg-do compile }
   { dg-options "-Wall" } */

char* f (void) __attribute__ ((malloc (__builtin_free)));

#if __cplusplus
extern "C" {
#endif

void free (void*);

#if __cplusplus
}
#endif

char* g (void) __attribute__ ((malloc (free)));


void test_nowarm (void)
{
  char *p = f ();
  free (p);

  p = g ();
  free (p);

  p = f ();
  __builtin_free (p);

  p = g ();
  __builtin_free (p);
}


void test_warn (void)
{
  char *p = f ();
  free (p + 1);               // { dg-warning "'(free|void free\\(void\\*\\))' called on pointer '(p|<unknown>)' with nonzero offset" }

  p = g ();
  free (p + 2);               // { dg-warning "'(free|void free\\(void\\*\\))' called on pointer '(p|<unknown>)' with nonzero offset" }

  p = f ();
  __builtin_free (p + 3);     // { dg-warning "'(__builtin_free|void __builtin_free\\(void\\*\\))' called on pointer '(p|<unknown>)' with nonzero offset" }

  p = g ();
  __builtin_free (p + 4);     // { dg-warning "'(__builtin_free|void __builtin_free\\(void\\*\\))' called on pointer '(p|<unknown>)' with nonzero offset" }
}
