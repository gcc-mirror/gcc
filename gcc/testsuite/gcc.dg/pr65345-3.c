/* PR c/65345 */
/* { dg-options "" } */

_Atomic float i = 3.0f;

float a1 = sizeof (i + 1.2);
float a2 = sizeof (i = 0);
float a3 = sizeof (i++);
float a4 = sizeof (i--);
float a5 = sizeof (-i);

float b1 = _Alignof (i + 1);
float b2 = _Alignof (i = 0);
float b3 = _Alignof (i++);
float b4 = _Alignof (i--);
float b5 = _Alignof (-i);

float c1 = i; /* { dg-error "initializer element is not constant" } */
float c2 = (i ? 1 : 2); /* { dg-error "initializer element is not constant" } */
float c3[(int) i]; /* { dg-error "variably modified" } */
float c4 = 0 || i; /* { dg-error "initializer element is not constant" } */
float c5 = (i += 10); /* { dg-error "initializer element is not constant" } */

_Static_assert (_Generic (i, float: 1, default: 0) == 1, "1");
_Static_assert (_Generic (i + 1, float: 1, default: 0) == 1, "2");
_Static_assert (_Generic (i = 0, float: 1, default: 0) == 1, "3");
_Static_assert (_Generic (i++, float: 1, default: 0) == 1, "4");
_Static_assert (_Generic (i--, float: 1, default: 0) == 1, "5");

_Atomic int sz = 2;
void fn1 (float a[sz + 1]);
void fn2 (float a[sz = 0]);
void fn3 (float a[sz++]);
void fn4 (float a[sz--]);
void fn5 (float a[-sz]);
