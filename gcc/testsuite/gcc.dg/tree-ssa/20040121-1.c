

/* Test that (p!=0) + (q!=0) is computed as int,
    not boolean */
/* { dg-options "-O3" } */
/* { dg-do run } */
extern void abort (void);
char *foo(char *p, char *q) {
    int x = (p !=0) + (q != 0);
    if (x==2)  return "a"; else return 0;
}
extern char *bar(char*, char*) __attribute__((noinline));
char *bar(char *first, char *last)
{
   int y;
   if (!first)  return last;
   if (!last)   return first;
   if (*first == 'a')
     return foo(first, last);
   return 0;
}
int
main() {
   char *p = "a", *q = "b";
   if (p)
     if (bar(p,q))
       return 0;
   abort();
}
