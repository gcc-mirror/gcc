/* { dg-do run } */
/* { dg-options "-fpermitted-flt-eval-methods=c11" } */

/* Test that we only see the C99/C11 values for __FLT_EVAL_METHOD__ if
   we are compiling with -fpermitted-flt-eval-methods=c11.  */

int main (int argc, char** argv)
{
  switch (__FLT_EVAL_METHOD__)
    {
      case 0:
      case 1:
      case 2:
      case -1:
	return 0;
      default:
	return 1;
    }
}
