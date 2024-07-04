/* { dg-additional-options "-std=gnu89" } */

void g();

f()
{
int x=1;
while(x)
{
x=h();
if(x)
g();
}
}
