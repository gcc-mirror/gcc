/* PR c/65345 */
/* { dg-do compile } */
/* { dg-options "" } */

_Atomic int i = 3;

int a1 = sizeof (i + 1);
int a2 = sizeof (i = 0);
int a3 = sizeof (i++);
int a4 = sizeof (i--);
int a5 = sizeof (-i);

int b1 = _Alignof (i + 1);
int b2 = _Alignof (i = 0);
int b3 = _Alignof (i++);
int b4 = _Alignof (i--);
int b5 = _Alignof (-i);

int c1 = i; /* { dg-error "initializer element is not constant" } */
int c2 = (i ? 1 : 2); /* { dg-error "initializer element is not constant" } */
int c3[i]; /* { dg-error "variably modified" } */
int c4 = 0 || i; /* { dg-error "initializer element is not constant" } */
int c5 = (i += 10); /* { dg-error "initializer element is not constant" } */

_Static_assert (_Generic (i, int: 1, default: 0) == 1, "1");
_Static_assert (_Generic (i + 1, int: 1, default: 0) == 1, "2");
_Static_assert (_Generic (i = 0, int: 1, default: 0) == 1, "3");
_Static_assert (_Generic (i++, int: 1, default: 0) == 1, "4");
_Static_assert (_Generic (i--, int: 1, default: 0) == 1, "5");

void fn1 (int a[i + 1]);
void fn2 (int a[i = 0]);
void fn3 (int a[i++]);
void fn4 (int a[i--]);
void fn5 (int a[-i]);
