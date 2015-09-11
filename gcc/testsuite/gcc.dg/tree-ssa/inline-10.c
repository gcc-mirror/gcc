/* { dg-do compile } */
/* { dg-options "-Os -fdump-tree-optimized -fno-partial-inlining" } */
void do_something1(void);
void do_something2(void);
void do_something3(void);
void do_something4(void);
void do_something5(void);
void do_something_big(int);

int do_something (int size)
{
  if (__builtin_constant_p (size))
    switch (size)
      {
	case 1:do_something1 (); break;
	case 2:do_something2 (); break;
	case 5:do_something1 ();  do_something1 ();
	case 3:do_something3 (); break;
	case 4:do_something4 (); break;
      }
  else
    do_something_big (size);
}
extern int n;
int
main()
{
  do_something (2);
  do_something (3);
  do_something (5);
  do_something (70);
}
/* All calls should be inlined, except for do_something (5).  */
/* { dg-final { scan-tree-dump-not "do_something1" "optimized" } } */
/* { dg-final { scan-tree-dump-times "do_something2" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "do_something3" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-times "do_something \\(5\\)" 1 "optimized" } } */
/* { dg-final { scan-tree-dump-not "do_something \\(70\\)" "optimized" } } */
