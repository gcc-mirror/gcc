/* This checks the gcc builtin macros defined to the byte
   sizes of C standard types.  */

int a[sizeof(int) == __SIZEOF_INT__ ? 1 : -1];
int b[sizeof(long) == __SIZEOF_LONG__ ? 1 : -1];
int c[sizeof(long long) == __SIZEOF_LONG_LONG__ ? 1 : -1];
int d[sizeof(short) == __SIZEOF_SHORT__ ? 1 : -1];
int e[sizeof(void *) == __SIZEOF_POINTER__ ? 1 : -1];
int f[sizeof(float) == __SIZEOF_FLOAT__ ? 1 : -1];
int g[sizeof(double) == __SIZEOF_DOUBLE__ ? 1 : -1];
int h[sizeof(long double) == __SIZEOF_LONG_DOUBLE__ ? 1 : -1];
int i[sizeof(__SIZE_TYPE__) == __SIZEOF_SIZE_T__ ? 1 : -1];
int j[sizeof(__WCHAR_TYPE__) == __SIZEOF_WCHAR_T__ ? 1 : -1];
int k[sizeof(__WINT_TYPE__) == __SIZEOF_WINT_T__ ? 1 : -1];
int l[sizeof(__PTRDIFF_TYPE__) == __SIZEOF_PTRDIFF_T__ ? 1 : -1];
