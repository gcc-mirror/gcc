// PR c++/43555
// { dg-options "" }
// { dg-do run }

extern "C" void * malloc (__SIZE_TYPE__);
extern "C" int printf (const char *, ...);
extern "C" void abort(void);

int nx,ny;

void f(double *x1d,int choice)
{
  double (*x2d)[nx][ny]=(double(*)[nx][ny])x1d;
  unsigned long delta;
//  (*x2d)[0][0]=123; // <- this line affects the result
  if (choice!=0)
  {
    delta=&(*x2d)[1][0]-x1d;
  }
  else
  {
    delta=&(*x2d)[1][0]-x1d;
  }
  printf("Choice: %d, Delta: %ld\n",choice,delta);
  if (delta != ny)
    abort ();
}

int main()
{
  double *data;
  nx=100;
  ny=100;
  data=(double*)malloc(nx*ny*sizeof(double));
  f(data,0);
  f(data,1);
  return 0;
}
