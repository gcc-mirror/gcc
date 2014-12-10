// { dg-do compile { target i?86-*-* x86_64-*-* } }
// { dg-options "-fschedule-insns2 -fsel-sched-pipelining -fselective-scheduling2 -fno-exceptions -O" }

struct QBasicAtomicInt
{
  int i, j;
  bool deref ()
  {
    asm volatile ("":"=m" (i), "=qm" (j));
  }
};

struct Data
{
  QBasicAtomicInt ref;
  void *data;
};

struct QByteArray
{
  Data * d;
  ~QByteArray ()
  {
    d->ref.deref ();
  }
};

int indexOf (unsigned);
int stat (void *, int *);
QByteArray encodeName ();

bool makeDir (unsigned len)
{
  unsigned i = 0;
  while (len)
    {
      int st;
      int pos = indexOf (i);
      QByteArray baseEncoded = encodeName ();
      if (stat (baseEncoded.d->data, &st) && stat (baseEncoded.d, &st))
        return false;
      i = pos;
    }
}
