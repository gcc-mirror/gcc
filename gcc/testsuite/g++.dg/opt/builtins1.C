// PR c++/14791
// Test if builtins with FILE * arguments work
// { dg-options "-O2 -Wformat" }

typedef struct _FILE FILE;
FILE *stderr;
extern "C" int printf (__const char *__restrict, ...);
extern "C" int fprintf (FILE *__restrict, __const char *__restrict, ...);

int main ()
{
  printf ("%d\n", 1, 1);		// { dg-warning "too many arguments for format" }
  fprintf (stderr, "%d\n", 1, 1);	// { dg-warning "too many arguments for format" }
}
