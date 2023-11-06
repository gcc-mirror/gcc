#define k(a) #a
char *s = k(k(1,2));
char *t = k(#) k(#undef k) k(x);

void f(void){}
