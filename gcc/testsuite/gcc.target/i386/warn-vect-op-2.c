/* { dg-do compile { target { ! ia32 } } }  */
/* { dg-options "-mno-sse -Wvector-operation-performance" }  */
#define vector(elcount, type)  \
__attribute__((vector_size((elcount)*sizeof(type)))) type

int main (int argc, char *argv[])
{
  vector (16, signed char) v0 = {argc, 1, 15, 38, 12, -1, argc, 2, 
				 argc, 1, 15, 38, 12, -1, argc, 2};
  vector (16, signed char) v1 = {-4, argc, 2, 11, 1, 17, -8, argc,
				 argc, 1, 15, 38, 12, -1, argc, 2};
  vector (16, signed char) res[] = 
  {
    v0 + v1,		  /* { dg-warning "expanded in parallel" }  */
    v0 - v1,              /* { dg-warning "expanded in parallel" }  */
    v0 > v1,              /* { dg-warning "expanded piecewise" }  */
    v0 & v1,              /* { dg-warning "expanded in parallel" }  */
    __builtin_shuffle (v0, v1),        /* { dg-warning "expanded piecewise" }  */
    __builtin_shuffle (v0, v1, v1)     /* { dg-warning "expanded piecewise" }  */
  };
 
  return res[argc][argc];
}
