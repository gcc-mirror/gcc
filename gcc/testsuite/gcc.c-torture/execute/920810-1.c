#include <stdio.h>
#include <string.h>
#include <stdlib.h>

typedef struct{void*super;int name;int size;}t;
t*f(t*clas,int size)
{
  t*child=(t*)malloc(size);
  memcpy(child,clas,clas->size);
  child->super=clas;
  child->name=0;
  child->size=size;
  return child;
}
int main(void)
{
  t foo,*bar;
  memset(&foo,37,sizeof(t));
  foo.size=sizeof(t);
  bar=f(&foo,sizeof(t));
  if(bar->super!=&foo||bar->name!=0||bar->size!=sizeof(t))abort();
  exit(0);
}
