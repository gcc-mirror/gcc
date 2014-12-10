// PR target/63815
// { dg-do run { target { { i?86-*-linux* x86_64-*-linux* } && lp64 } } }
// { dg-options "-mcmodel=large" }
// { dg-additional-options "-fpic" { target fpic } }

struct ICCStringClass
{
  virtual int CreateString (int) = 0;
};

struct AGSCCDynamicObject
{
  virtual void Unserialize () = 0;
};

struct ScriptString:AGSCCDynamicObject, ICCStringClass
{
  virtual int CreateString (int);
  virtual void Unserialize ();
};

int
__attribute__ ((noinline))
CreateNewScriptString (int fromText, bool reAllocate = true)
{
  return fromText;
}

int
__attribute__ ((noinline))
ScriptString::CreateString (int fromText)
{
  return CreateNewScriptString (fromText);
}

void
__attribute__ ((noinline))
ScriptString::Unserialize ()
{
}

int
main ()
{
  ICCStringClass *x = new ScriptString;

  if (x->CreateString (1) != 1)
    __builtin_abort ();
  return 0;
}
