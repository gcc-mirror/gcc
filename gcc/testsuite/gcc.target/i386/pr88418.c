/* PR target/88418 */
/* { dg-do compile } */
/* { dg-options "-O1 -fpack-struct -msse4.1 -mno-avx" } */

typedef long long v2di __attribute__ ((__vector_size__ (16)));

union df {
  v2di se[2];
};

void
qg (union df *jz, union df *pl)
{
  jz->se[0] = jz->se[0] == pl->se[0];
}
