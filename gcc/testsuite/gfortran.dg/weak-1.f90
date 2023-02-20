! { dg-do compile }
! { dg-require-weak "" }
! { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?impl" { target { ! nvptx-*-* } } } }
! { dg-final { scan-assembler-times "\\.weak \\.func impl" 2 { target nvptx-*-* } } }
subroutine impl
!GCC$ ATTRIBUTES weak :: impl
end subroutine
