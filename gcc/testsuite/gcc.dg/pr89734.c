/* PR c/89734 */
/* { dg-do compile } */
/* { dg-options "" } */

typedef const int CI;
typedef _Atomic int AI;

CI foo (void);
const int foo (void);

AI baz (void);
_Atomic int baz (void);
