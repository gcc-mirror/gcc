! { dg-do run }
! { dg-options "-fdec" }
!
! Test support for providing a default exponent of zero when unspecified in
! real constants with -fdec.
!

subroutine asserteq (rexp, ract, msg)
  real, intent(in) :: rexp, ract
  character(*), intent(in) :: msg
  if (rexp .ne. ract) then
    write (*, '(A,F12.6,F12.6)') msg, rexp, ract
    call abort()
  endif
end subroutine

implicit none

real, parameter :: r1 = 8e0
real, parameter :: r2 = 8e ! { equivalent to 8e0 }
real, volatile :: r3, r4
character(2) :: s
r3 = 8e ! { equivalent to 8e0 }
s = '8e'

read (s, *) r4

call asserteq (r1, r2, "[const]")
call asserteq (r1, r3, "[vol. ]")
call asserteq (r1, r4, "[read ]")

r4 = 8e + 48e
call asserteq (56e, r4, "[sum ]")

end
