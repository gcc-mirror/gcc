/* { dg-do run } */
/* { dg-options "-O" } */
/* { dg-shouldfail } */

/* Check that the third compare won't be pulled ahead of the second one and
   prevent, which would prevent the NULL pointer dereference that should cause
   the execution to fail.  */

struct s {
  char a, b;
  int *p;
};

struct s a = { 0, 1, 0 };
struct s b = { 0, 0, 0 };

int f () {
  return (a.a != b.a
	  || *b.p != *a.p
	  || a.b != b.b);
}

int main() {
  f ();
  return 0;
}
