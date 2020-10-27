! { dg-do compile }
! PR97491 - Wrong restriction for VALUE arguments of pure procedures

pure function foo (x) result (ret)
  integer        :: ret
  integer, value :: x
  x = x / 2
  ret = x
end function foo

elemental function foo1 (x)
  integer        :: foo1
  integer, value :: x
  x = x / 2
  foo1 = x
end function foo1
