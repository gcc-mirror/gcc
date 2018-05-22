// { dg-additional-options -fmodules-atom }
import kevin;

#if 0 // { dg-message "ended immediately before" }
#if 1
import kevin;
#endif
#elif 1
int i;
#endif

int j; // end here
