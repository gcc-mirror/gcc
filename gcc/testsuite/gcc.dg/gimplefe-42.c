/* { dg-do compile } */
/* { dg-options "-fgimple" } */

typedef char ref_all_char __attribute__((may_alias));
char a[7];
__GIMPLE void f()
{
  int _1;
  /* string literals inside __MEM need their address taken.  */
  __MEM <char[7]> ((ref_all_char *)&a)
    = __MEM <char[7]> (_Literal (char *) &"654321");
  /* but plain assignment also works.  */
  __MEM <char[7]> ((ref_all_char *)&a) = "654321";
  /* also punning with int.  */
  _1 = __MEM <int> (_Literal (char *) &"654321");
  __MEM <int> ((ref_all_char *)&a) = _1;
  return;
}
