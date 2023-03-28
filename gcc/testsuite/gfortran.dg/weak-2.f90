! { dg-do compile }
! { dg-require-weak "" }
! { dg-skip-if "" { x86_64-*-mingw* } }

! 1.
! { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?__foo_MOD_abc" { target { ! nvptx-*-* } } } }
! { dg-final { scan-assembler-times "\\.weak \\.global \\.align 4 \\.u32 __foo_MOD_abc" 1 { target nvptx-*-* } } }
module foo
implicit none
!GCC$ ATTRIBUTES weak :: abc
real :: abc(7)
end module

! 2.
! { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?impl1" { target { ! nvptx-*-* } } } }
! { dg-final { scan-assembler-times "\\.weak \\.func \\(\\.param\\.u32 %value_out\\) impl1" 2 { target nvptx-*-* } } }
integer function impl1()
implicit none
!GCC$ ATTRIBUTES weak :: impl1
end function

! 3.
! { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?bar__" { target { ! nvptx-*-* } } } }
! { dg-final { scan-assembler-times "\\.weak \\.func \\(\\.param\\.u32 %value_out\\) bar__" 2 { target nvptx-*-* } } }
integer function impl2() bind(c,name='bar__')
implicit none
!GCC$ ATTRIBUTES weak :: impl2
end function
