// PR 30168
// { dg-do compile }
// { dg-options "-O2" }

struct aaa
{
      aaa(_Complex float __z) ;
      _Complex float _M_value;
};
aaa::aaa(_Complex float __z)
{
  __z*=2.0f;
  _M_value = __z;
}
