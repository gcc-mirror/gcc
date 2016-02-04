/* Test handling of pointers to arrays of const elements involving a
   typedef.  PR c/68162.  */

typedef const double cd;
void f (const double (*)[]);
void g (void) { f ((cd (*)[]) 0); }
