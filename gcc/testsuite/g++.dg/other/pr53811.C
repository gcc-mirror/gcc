// { dg-do compile }
// { dg-options "-mcmodel=large" { target { { i?86-*-* x86_64-*-* } && lp64 } } }

struct ICCStringClass
{
  virtual void *
  CreateString (const char *fromText) = 0;
};

struct AGSCCDynamicObject
{
  virtual void
  Unserialize (int index, const char *serializedData, int dataSize) = 0;
};


struct ScriptString:AGSCCDynamicObject, ICCStringClass
{
  virtual void *CreateString (const char *fromText);
};

const char *
CreateNewScriptString (const char *fromText, bool reAllocate = true);

void *
ScriptString::CreateString (const char *fromText)
{
  return (void *) CreateNewScriptString (fromText);
}
