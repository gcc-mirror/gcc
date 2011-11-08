// { dg-do compile }
// { dg-options "-fgnu-tm" }

void* operator new(__SIZE_TYPE__) throw (int);

void *point;

void funky()
{
  point = new (int);
}
