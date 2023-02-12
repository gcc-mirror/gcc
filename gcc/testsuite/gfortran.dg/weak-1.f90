! { dg-do compile }
! { dg-require-weak "" }
! { dg-final { scan-assembler "weak\[^ \t\]*\[ \t\]_?impl" } }
subroutine impl
!GCC$ ATTRIBUTES weak :: impl
end subroutine
