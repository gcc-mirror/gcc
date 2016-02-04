/* { dg-do compile { target { ! ia32 } } }  */
/* { dg-options "-mno-sse -Wvector-operation-performance" }  */
#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

int main (int argc, char *argv[])
{
  vector (4, int) v0 = {argc, 1, 15, 38};
  vector (4, int) v1 = {-4, argc, 2, 11};
  vector (4, int) res[] = 
  {
    v0 + v1,	  /* { dg-warning "expanded piecewise" }  */
    v0 - v1,	  /* { dg-warning "expanded piecewise" }  */
    v0 > v1,	  /* { dg-warning "expanded piecewise" }  */
    v0 & v1,	  /* { dg-warning "expanded in parallel" }  */
    __builtin_shuffle (v0, v1),	    /* { dg-warning "expanded piecewise" }  */
    __builtin_shuffle (v0, v1, v1)  /* { dg-warning "expanded piecewise" }  */  
  };

  return res[argc][argc];
}
