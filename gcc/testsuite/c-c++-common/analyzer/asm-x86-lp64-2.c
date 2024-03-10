/* { dg-do compile { target x86_64-*-* } } */
/* { dg-require-effective-target lp64 } */

/* Adapted from Linux x86: page_ref_dec_and_test.c (GPL-2.0).  */

#ifndef __cplusplus 
  typedef _Bool bool;
#endif

typedef struct {
  int counter;
} atomic_t;

bool
arch_atomic_dec_and_test(atomic_t *v) {
  return ({
    bool c;
    asm volatile(".pushsection .smp_locks,\"a\"\n"
                 ".balign 4\n"
                 ".long 671f - .\n"
                 ".popsection\n"
                 "671:"
                 "\n\tlock; "
                 "decl"
                 " "
                 "%[var]"
                 "\n\t/* output condition code "
                 "e"
                 "*/\n"
                 : [ var ] "+m"(v->counter), "=@cc"
                                             "e"(c)
                 :
                 : "memory");
    c;
  });
}
