! { dg-do compile }
! PR 31821
program main
  character (len=4), pointer:: s1
  character (len=20), pointer :: p1
  character (len=4) :: c
  s1 = 'abcd'
  p1 => s1(2:3) ! { dg-error "Unequal character lengths \\(20/2\\)" }
  p1 => c(1:) ! { dg-error "Unequal character lengths \\(20/4\\)" }
  p1 => c(:4) ! { dg-error "Unequal character lengths \\(20/4\\)" }
end
