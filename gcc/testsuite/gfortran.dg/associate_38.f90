! { dg-do run }
!
! Test the fix for PR49636 in which the 'span' of 'ty1' was not used
! in the descriptor of 'i'.
!
! Contributed by Fred Krogh  <fkrogh#gcc@mathalacarte.com>
!
program test
  type ty1
    integer :: k
    integer :: i
  end type ty1
  type ty2
    type(ty1) :: j(3)
  end type ty2

  type(ty2) t2
  t2%j(1:3)%i = [ 1, 3, 5 ]
  associate (i=>t2%j%i)
    if (any (t2%j(1:3)%i .ne. i(1:3))) stop 1
  end associate
end program test
