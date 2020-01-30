// { dg-additional-options "-fmodules-ts -fdump-lang-module-eh" }
import kevin;

#if 1
#if 1
import kevin;
#endif
#elif 1
int i;
#endif

int j; // end here

// No duplicate mapper request.
// FIXME: need to rejig the mapper request queueing post-1857
// { dg-final { scan-lang-dump "Mapper request:\\+IMPORT kevin\n-\n" module { xfail *-*-* } } }
