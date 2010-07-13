// { dg-do run }

struct Lower {
    const int& ref;
    Lower(const int& ref) : ref(ref) { }
};
struct Middle : public virtual Lower {
    Middle(const int& ref) : Lower(ref) { }
};
struct Upper : public Middle {
    Upper(const int& ref) : Lower(ref), Middle(ref) { }
    int get() { return ref; }
};
extern "C" void abort (void);
int main()
{
  int i = 0;
  Upper upper(i);
  if (upper.get() != 0)
    abort ();
  return 0;
}
