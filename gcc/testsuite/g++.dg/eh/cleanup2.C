// PR c++/12491
// { dg-do compile }
// { dg-options "-O2" }

// The return statements are necessary to trigger this bug.

class Object
{
public:
  virtual ~Object (void) { return; }
};

class AutoPtr
{
public:
  ~AutoPtr (void) { delete m_rep; return; }
private:
  const Object *m_rep;
};

class Handle
{
public:
  ~Handle (void) { return; }
private:
    AutoPtr m_rep;
};

class HandleOf:public Handle
{
public:
  ~HandleOf (void) { return; }
};

class Error
{
public:
  ~Error (void);
private:
  HandleOf m_hndl;
};

Error::~Error (void)
{
  return;
}
