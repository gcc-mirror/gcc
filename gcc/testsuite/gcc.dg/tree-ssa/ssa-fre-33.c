/* { dg-do compile } */
/* { dg-options "-O -fdump-tree-fre1-details" } */
/* { dg-options "-O -fdump-tree-fre1-details -fno-common" { target hppa*-*-hpux* } } */

#define vector __attribute__((vector_size(16) ))

struct {
    float i;
    vector float global_res;
} s;
float x;
int main(int argc)
{
  vector float res = (vector float){0.0f,0.0f,0.0f,1.0f};
  res += (vector float){1.0f,2.0f,3.0f,4.0f};
  s.global_res = res;
  x = *((float*)&s.global_res + 1);
  return 0;
}

/* { dg-final { scan-tree-dump "Replaced BIT_FIELD_REF.*with 2" "fre1" } } */
/* { dg-final { cleanup-tree-dump "fre1" } } */
