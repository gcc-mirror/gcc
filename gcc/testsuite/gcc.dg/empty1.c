/* { dg-do run } */
/* { dg-options "" } */

struct S { int : 0; };
union U { int : 0; };

int main () {
  if (__alignof__ (struct S) != __alignof__ (union U))
    return 1;
  return 0;
}
