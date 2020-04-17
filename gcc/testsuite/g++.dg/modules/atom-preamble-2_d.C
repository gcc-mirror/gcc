// { dg-additional-options "-fmodules-ts" }
import kevin;

#if 1
#if 1
import kevin;
#endif
#elif 1
int i;
#endif

int j; // end here
