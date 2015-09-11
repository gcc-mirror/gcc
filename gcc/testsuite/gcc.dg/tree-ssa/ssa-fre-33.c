/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */
/* { dg-additional-options "-fno-common" { target hppa*-*-hpux* } } */

#define vector __attribute__((vector_size(16) ))

struct {
    float i;
    vector float global_res;
} s;
float x;
int main(int argc)
{
  vector float res;
  res = (vector float){1.0f,2.0f,3.0f,5.0f};
  s.global_res = res;
  x = *((float*)&s.global_res + 1);
  return 0;
}

/* { dg-final { scan-tree-dump "Replaced BIT_FIELD_REF.*with 2" "fre1" } } */
