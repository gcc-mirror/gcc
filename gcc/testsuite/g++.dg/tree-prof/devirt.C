/* PR ipa/88561 */
/* { dg-options "-O3 -fdump-tree-tracer-details -fdump-tree-dom3-details -fno-profile-values" } */

struct nsISupports
{
  virtual int QueryInterface (const int &aIID, void **aInstancePtr) = 0;
  virtual __attribute__((noinline, noclone)) unsigned AddRef (void) = 0;
  virtual unsigned Release (void) = 0;
};

struct nsIObserver : public nsISupports
{
  virtual int Observe (nsISupports * aSubject, const char *aTopic, const unsigned short *aData) = 0;
};

struct nsISupportsWeakReference : public nsISupports
{
  virtual int GetWeakReference (void **_retval) = 0;
};

struct nsSupportsWeakReference : public nsISupportsWeakReference
{
  nsSupportsWeakReference () : mProxy (0) {}
  virtual int GetWeakReference (void **_retval) override { return 0; }
  ~nsSupportsWeakReference () {}
  void NoticeProxyDestruction () { mProxy = nullptr; }
  void *mProxy;
  void ClearWeakReferences ();
  bool HasWeakReferences () const { return !!mProxy; }
};

struct mozIPersonalDictionary : public nsISupports
{
  virtual int Load (void) = 0;
  virtual int Save (void) = 0;
  virtual int GetWordList (void **aWordList) = 0;
  virtual int Check (const int &word, bool * _retval) = 0;
  virtual int AddWord (const int &word) = 0;
  virtual int RemoveWord (const int &word) = 0;
  virtual int IgnoreWord (const int &word) = 0;
  virtual int EndSession (void) = 0;
};

struct mozPersonalDictionary final
  : public mozIPersonalDictionary, public nsIObserver, public nsSupportsWeakReference
{
  virtual int QueryInterface (const int &aIID, void **aInstancePtr) override;
  virtual __attribute__((noinline, noclone)) unsigned AddRef (void) override;
  virtual unsigned Release (void) override;
  unsigned long mRefCnt;
  virtual int Load (void) override { return 0; }
  virtual int Save (void) override { return 0; }
  virtual int GetWordList (void **aWordList) override { return 0; }
  virtual int Check (const int &word, bool * _retval) override { return 0; }
  virtual int AddWord (const int &word) override { return 0; }
  virtual int RemoveWord (const int &word) override { return 0; }
  virtual int IgnoreWord (const int &word) override { return 0; }
  virtual int EndSession (void) override { return 0; }
  virtual int Observe (nsISupports * aSubject, const char *aTopic, const unsigned short *aData) override { return 0; }
  mozPersonalDictionary () : mRefCnt(0) {}
  int Init () { return 0; }
  virtual ~mozPersonalDictionary () {}
  bool mIsLoaded;
  bool mSavePending;
  void *mFile;
  char mMonitor[96];
  char mMonitorSave[96];
  char mDictionaryTable[32];
  char mIgnoreTable[32];
};

unsigned
mozPersonalDictionary::AddRef (void)
{
  unsigned count = ++mRefCnt;
  return count;
}

unsigned
mozPersonalDictionary::Release (void)
{
  unsigned count = --mRefCnt;
  if (count == 0)
    {
      mRefCnt = 1;
      delete (this);
      return 0;
    }
  return count;
}

int
mozPersonalDictionary::QueryInterface (const int &aIID, void **aInstancePtr)
{
  nsISupports *foundInterface;
  if (aIID == 122)
    foundInterface = static_cast <mozIPersonalDictionary *>(this);
  else
    foundInterface = static_cast <nsISupportsWeakReference *>(this);
  int status;
  foundInterface->AddRef ();
  *aInstancePtr = foundInterface;
  return status;
}

__attribute__((noipa)) int
foo (nsISupports *p, const int &i)
{
  void *q;
  return p->QueryInterface (i, &q);
}

int
main ()
{
  mozPersonalDictionary m;
  int j = 123;
  for (int i = 0; i < 100000; i++)
    foo (static_cast <nsISupportsWeakReference *>(&m), j);
  if (m.mRefCnt != 100000)
    __builtin_abort ();
}

/* { dg-final-use-not-autofdo { scan-tree-dump-times "folding virtual function call to virtual unsigned int mozPersonalDictionary::_ZThn16" 1 "tracer" { target { lp64 || llp64 } } } } */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "folding virtual function call to virtual unsigned int mozPersonalDictionary::_ZThn8" 1 "tracer" { target ilp32 } } } */
/* { dg-final-use-not-autofdo { scan-tree-dump-times "folding virtual function call to virtual unsigned int mozPersonalDictionary::AddRef" 1 "tracer" } } */
