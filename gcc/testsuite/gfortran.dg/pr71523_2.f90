! PR Fortran/71523
!
! { dg-do run }
! { dg-options "-finit-integer=12345 -fautomatic -fmax-stack-var-size=8" }
!
! Make sure that variables larger than max-stack-var-size become
! static and are given the correct _static_ initializer.
!

function set(idx, val)
  implicit none
  integer, intent(in) :: idx, val
  integer set
  integer arr(100)

  set = arr(idx)
  arr(idx) = val
  return
end function

  integer set, val

  val = set(1, 5)
  if (val .ne. 12345) then
    call abort()
  endif

  val = set(1, 10)
  if (val .ne. 5) then
    call abort()
  endif

  val = set(1, 100)
  if (val .ne. 10) then
    call abort()
  endif

end
