! { dg-do compile }
! This checks the fix for PR33295 in which the A_type in initA was
! not promoted to module level and so not recognised as being the
! same as that emanating directly from module a. 
!
! Contributed by Janus Weil <jaydub66@gmail.com>
!
module A
  type A_type
    real comp
  end type
end module A

module B
contains
  function initA()
    use A
    implicit none
    type(A_type):: initA
    initA%comp=1.0
  end function
end module B

program C
  use B
  use A
  implicit none
  type(A_type):: A_var
  A_var = initA()
end program C

! { dg-final { cleanup-modules "a b" } }

