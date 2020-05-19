/* { dg-do compile } */
/* { dg-options "-O -finline-functions -finline-small-functions -Wuninitialized" }  */
/* { dg-add-options bind_pic_locally } */

typedef int int32_t __attribute__((mode (__SI__)));

struct SQObjectPtr
{
  int32_t _type;
  SQObjectPtr operator = (long);
};
struct SQObjectPtrVec
{
  SQObjectPtr fff (unsigned);
  SQObjectPtr *_vals;
};

struct SQInstruction
{
  int _arg1;
  unsigned op;
  unsigned _arg0;
  unsigned _arg2;
};
struct SQVM
{
  struct CallInfo
  {
    SQInstruction *_ip;
  };
    bool
    Execute (SQObjectPtr &, long, long, long, SQObjectPtr &, unsigned, int);
    bool
    FOREACH_OP
    (SQObjectPtr
     &, SQObjectPtr &, SQObjectPtr &, SQObjectPtr &, long, int, int &);
  SQObjectPtrVec _stack;
  CallInfo *ci;
  long _nnativecalls;
  bool ShouldSuspend ();
};
struct AutoDec
{
  AutoDec (long *);
   ~AutoDec ();
};
bool
  SQVM::FOREACH_OP
  (SQObjectPtr
   &
   o1,
   SQObjectPtr & o2, SQObjectPtr &, SQObjectPtr & o4, long, int, int &jump)
{
  long
    nrefidx = 0;
  switch (o1._type)
    {
    case 0x02000000L:
      o4 = nrefidx;
      jump = 1;
      return true;
    case 0x00000080L:
      {
	long
	  idx = 10;
	o2 = idx;
	jump = 0;
      }
    }

  return false;
}

bool
  SQVM::Execute
  (SQObjectPtr &, long, long, long, SQObjectPtr &, unsigned, int)
{
  AutoDec
  ad (&_nnativecalls);
  for (;;)
    {
      if (ShouldSuspend ())
	return true;
      SQInstruction
	_i_ = *ci->_ip;
      switch (_i_.op)
	{
	case 0:
	  {
	    int
	      tojump;

	    FOREACH_OP
	      (_stack._vals
	       [_i_._arg0],
	       _stack._vals
	       [_i_._arg2],
	       _stack._vals
	       [_i_._arg2],
	       _stack._vals[_i_._arg2], _i_._arg2, _i_._arg1, tojump);

	    ci += tojump;  /* { dg-warning "uninitialized" "warning" }  */
	  }
	case 1:
	  _stack.fff (_i_._arg1);
	}
    }

  return true;
}
