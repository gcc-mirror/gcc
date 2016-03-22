// { dg-do compile }
// { dg-additional-options "-w -Wno-psabi" }

typedef int vec __attribute__((vector_size(64)));
vec f(vec x,vec y,vec z)
{
  vec zero={};
  vec one=zero+1;
  vec c=x<y;
  return z+(c?one:zero);
}
