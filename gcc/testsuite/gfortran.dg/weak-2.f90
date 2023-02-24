! { dg-do compile }
! { dg-require-weak "" }
! { dg-skip-if "" { x86_64-*-mingw* } }
! { dg-skip-if "" { nvptx-*-* } }

! 1.
! { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?__foo_MOD_abc" } }
module foo
implicit none
!GCC$ ATTRIBUTES weak :: abc
real :: abc(7)
end module

! 2.
! { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?impl1" } }
integer function impl1()
implicit none
!GCC$ ATTRIBUTES weak :: impl1
end function

! 3.
! { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?bar__" } }
integer function impl2() bind(c,name='bar__')
implicit none
!GCC$ ATTRIBUTES weak :: impl2
end function
