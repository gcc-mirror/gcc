// { dg-do compile }
// { dg-additional-options "-fPIC -fstack-protector-strong" { target fpic } }
// { dg-require-effective-target fstack_protector }

struct struct0
{
  struct0 (void(*)());
};

int func5 (int* ptr);
void func3 (int*, struct0*);

inline void
func4 (int* a, void (*b)())
{
  if (func5 (a) != 2)
  {
    struct0 f (b);
    func3(a, &f);
  }
}

struct struct1
{
  const void* val0;
  const void* val1;
};

void* func3 (const void*);

static const void* gvar1 = 0;
static const void* gvar2 = 0;
static int gvar0 = 0;

void
func0 (void)
{
  gvar2 = func3 (gvar1);
}

inline void
func1 (void)
{
  func4 (&gvar0, &func0);
}

struct1 func2 (void)
{
  func1 ();
  struct1 s;
  s.val0 = gvar1;
  s.val1 = gvar2;
  return s;
}
