// { dg-additional-options "-fmodules-ts" }
import kevin;

#if 0
#if 1
import kevin;
#endif
#elif 1
import kevin;
#endif

int i; // end here
