#include <stdarg.h>

typedef struct{double x,y;}point;
point pts[]={{1.0,2.0},{3.0,4.0},{5.0,6.0},{7.0,8.0}};
static int va1(int nargs,...)
{
  va_list args;
  int i;
  point pi;
  va_start(args,nargs);
  for(i=0;i<nargs;i++){
    pi=va_arg(args,point);
    if(pts[i].x!=pi.x||pts[i].y!=pi.y)abort();
  }
  va_end(args);
}

typedef struct{int x,y;}ipoint;
ipoint ipts[]={{1,2},{3,4},{5,6},{7,8}};
static int va2(int nargs,...)
{
  va_list args;
  int i;
  ipoint pi;
  va_start(args,nargs);
  for(i=0;i<nargs;i++){
    pi=va_arg(args,ipoint);
    if(ipts[i].x!=pi.x||ipts[i].y!=pi.y)abort();
  }
  va_end(args);
}

main()
{
va1(4,pts[0],pts[1],pts[2],pts[3]);
va2(4,ipts[0],ipts[1],ipts[2],ipts[3]);
exit(0);
}
