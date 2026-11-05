// PR target/128437
// { dg-do compile { target { *-*-linux* && { ! ia32 } } } }
// { dg-options "-O2 -fPIC -fvisibility=hidden -mtls-dialect=gnu" }
// { dg-require-effective-target c++11 }

void f ();
void *p;
struct { void *slot; } *entries;
long capacity;

void erase ()
{
  if (capacity)
    return;
  for (int i = 0; entries[i].slot; ++i)
    if (!entries)
      entries = {};
}

thread_local int current, x, y;

void released (void *)
{
  if (current)
    erase ();
}

struct Ref
{
  int v;
  ~Ref () { released (&v); }
};

void replace (int, Ref)
{
  if (p)
    f ();
}

void g ()
{
  replace (x, {});
  replace (y, {});
}
