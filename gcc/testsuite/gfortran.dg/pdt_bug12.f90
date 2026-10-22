! { dg-do run }
! Neither gfc nor ifx give the expected tag = 77 > Is this test correct?
! F2023 19.6.5(24) "Invocation of a procedure that has a nonpointer
! nonallocatable INTENT (OUT) dummy argument causes all nonpointer
! default-initialized subcomponents of the dummy argument to become defined.
! See attachment to PR82173 comment 10 - reproducers/bug12-intent-out-default-init.f90
module mk
  type :: s
    integer :: i
    real :: j
  end type

  type :: t(n)
    integer, len :: n = 1
    real :: v(n)
    integer(4) :: tag = 77
    integer(4) :: arr(2) = [1_4 , 2_4]
    type(s) :: der = s (1, 2.0)
  end type
contains
  subroutine reset (a , a2)
    type(t(3)), intent(out) :: a
    type(t(3)), optional, intent(out) :: a2
    if (a%tag /= 77) a%tag = 0
    if (present (a2)) then
      if (a2%tag /= 77) a2%tag = 0          ! Reference optional
    end if
  end subroutine
  subroutine resetp (a)
    type(t(*)), intent(out) :: a
    if (a%tag /= 77) a%tag = 0
  end subroutine
  subroutine reset_array (a, a2)
    type(t(3)), intent(out) :: a(:)
    type(t(3)), optional, intent(out) :: a2(:)
    if (any (a%tag /= 77)) a%tag = 0
    if (present (a2)) then
      if (any (a2%tag /= 77)) a2%tag = 0    ! ditto
    end if
  end subroutine
end module
program k1
  use mk
  type(t(3)) :: a, opt
  type(t(3)) :: b(2), b2(2)
  a%tag = 5
  a%v = 0.0
  opt = a
  b = a
  b2 = b

! First with missing optional argument:
  call reset (a)
  if (a%tag /= 77) stop 1
  if (any (a%arr /= [1_4,2_4])) stop 2
  if ((a%der%i /= 1) .or. (int (100 * a%der%j) /= 200)) stop 3
  a%tag = 5

! Then with the missing argument:
  call reset (a, opt)
  if (opt%tag /= 77) stop 4
  if (any (opt%arr /= [1_4,2_4])) stop 5
  if ((opt%der%i /= 1) .or. (int (100 * opt%der%j) /= 200)) stop 6
  a%tag = 5

! Then and assumed parameter:
  call resetp (a)
  if (a%tag /= 77) stop 7
  if (any (a%arr /= [1_4,2_4])) stop 8
  if ((a%der%i /= 1) .or. (int (100 * a%der%j) /= 200)) stop 9

! Finally with an array
  call reset_array (b)
  if (any (b%tag /= 77)) stop 10
  if (any (b(1)%arr /= [1_4,2_4]) .or. any (b(2)%arr /= [1_4,2_4])) stop 11
  if (any (b%der%i /= 1) .or. any (int (100 * b%der%j) /= 200)) stop 12

! Again with the optional argument
  call reset_array (b, b2)
  if (any (b2%tag /= 77)) stop 13
  if (any (b2(1)%arr /= [1_4,2_4]) .or. any (b2(2)%arr /= [1_4,2_4])) stop 14
  if (any (b2%der%i /= 1) .or. any (int (100 * b2%der%j) /= 200)) stop 15
end program
