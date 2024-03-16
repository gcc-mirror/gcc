/* { dg-additional-options "-std=gnu89" } */

typedef struct{struct{char*d;int b;}*i;}*t;
double f();
g(p)t p;
{
  short x,y,delta,s,w,h,fx,fy,tx,ty;
  int q1,q2,q3,q4;
  h=f((ty-fy)/2.0+0.5);
  s=(((int)((short)(tx-fx))<(int)((short)(ty-fy)))?((short)(tx-fx)):((short)(ty-fy)))%2;
  delta=(((int)(w)<(int)(h))?(w):(h))-s;
  for(x=0;x<=delta;x++)
    for(y=1-s;y<=delta;y++){
      q1=((int)((*(p->i->d+(fx+w+x)/8+(fy+h+y)*p->i->b)&(1<<((fx+w+x)%8)))?1:0));
      q2=((int)((*(p->i->d+(fx+w+y)/8+(fy+h-s-x)*p->i->b)&(1<<((fx+w+y)%8)))?1:0));
      q3=((int)((*(p->i->d+(fx+w-s-x)/8+(fy+h-s-y)*p->i->b)&(1<<((fx+w-s-x)%8)))?1:0));
      q4=((int)((*(p->i->d+(fx+w-s-y)/8+(fy+h+x)*p->i->b)&(1<<((fx+w-s-y)%8)))?1:0));
      if(q4!=q1)
	ff(p,fx+w-s-y,fy+h+x);
    }
}
