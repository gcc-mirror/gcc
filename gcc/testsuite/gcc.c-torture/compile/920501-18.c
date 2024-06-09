/* { dg-additional-options "-std=gnu89" } */

union u{int i;float f;};
x(p)int p;{union u x;for(x.i=0;x.i<p;x.i++)if(x.f>(float)3.0)break;}
