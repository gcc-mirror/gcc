/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */
/* { dg-options "-O -fdump-tree-fre1-details -fno-common" { target hppa*-*-hpux* } } */

#define vector __attribute__((vector_size(16) ))

struct {
    float i;
    vector float global_res;
} s;
float foo(float f)
{
  vector float res = (vector float){0.0f,f,0.0f,1.0f};
  s.global_res = res;
  return *((float*)&s.global_res + 1);
}

/* { dg-final { scan-tree-dump "Replaced BIT_FIELD_REF.*with f" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
