/* The lower-subreg pass would ICE on this test case with
   TODO_verify_flow on 32-bit PowerPC.  It didn't handle REG_EH_REGION
   notes correctly.  This is reduced from interpret.cc in libjava.  */

/* { dg-do compile } */
/* { dg-options "-O2 -fnon-call-exceptions" } */

union _Jv_word
{
  double *o;
  int i;
  int ia[1];
};
union _Jv_word2
{
  int ia[2];
  double d;
};

class _Jv_InterpMethod
{
  static void run_debug (_Jv_word *);
};

void
_Jv_InterpMethod::run_debug (_Jv_word *sp)
{
  try
  {
    {
      int index = ((int) (--sp)->i);
      double *arr = (double *) ((--sp)->o);
      _Jv_word2 w2;
      w2.d = arr[index];
      (sp++)->ia[0] = w2.ia[0];
    }
  }
  catch (int * ex)
  {
  }
}
