/* { dg-do compile } */

void a(int arg)
{
  extern struct { int x; } obj;
  setjmp();
  obj.x = arg;
  arg = arg;
  if (obj.x)
    func();
  if (obj.x)
    func();
}
