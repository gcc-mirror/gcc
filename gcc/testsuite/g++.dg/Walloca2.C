// { dg-do compile }
// { dg-options "-Walloca-larger-than=4207115063 -Wvla-larger-than=1233877270 -O2 --param ggc-min-heapsize=0 --param ggc-min-expand=0 -w" }

int a;
char *b = static_cast<char *>(__builtin_alloca (a));
