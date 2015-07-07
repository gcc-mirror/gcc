/* { dg-do compile } */
/* { dg-options "-fdump-ada-spec" } */

template<class T, class U> class Generic_Array
{
  Generic_Array();
};

template class Generic_Array<char, int>;

/* { dg-final { scan-ada-spec-not "access Generic_Array" } } */
/* { dg-final { cleanup-ada-spec } } */
