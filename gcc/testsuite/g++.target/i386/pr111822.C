/* PR target/111822 */
/* { dg-do compile { target ia32 } } */
/* { dg-options "-O2 -flive-range-shrinkage -fno-dce -fnon-call-exceptions -march=x86-64" } */

typedef union {
  int *pNativeClosure;
  struct SQRefCounted *pRefCounted;
} SQObjectValue;
typedef struct {
  SQObjectValue _unVal;
} SQObject;
typedef long SQFUNCTION(struct SQVM *);
struct SQVM {
  void CallNative();
};
struct SQRefCounted {
  long long _uiRef;
};
void Null();
struct SQObjectPtr : SQObject {
  SQObjectPtr() {}
  SQObjectPtr(int *pNativeClosure) {
    _unVal.pNativeClosure = pNativeClosure;
    _unVal.pRefCounted->_uiRef++;
  }
  ~SQObjectPtr() { --_unVal.pRefCounted->_uiRef; }
};
struct CallInfo {
  SQObjectPtr _closure;
};
long long _top;
SQFUNCTION _function;
int *CallNative_nclosure;
void SQVM::CallNative() {
  long long oldtop = _top;
  CallInfo lci;
  lci._closure = CallNative_nclosure;
  try {
    _function(this);
  } catch (...) {
    _top = oldtop;
  }
  while (oldtop)
    Null();
}
