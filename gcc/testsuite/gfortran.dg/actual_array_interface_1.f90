! { dg-do compile }
! Tests the fix for PR29490, in which the creation of the
! interface expression for the first argument of the call to
! 'john' would cause an ICE because GFC_TYPE_ARRAY_LBOUND
! was NULL.
!
! Contributed by Philip Mason <pmason@ricardo.com>
!
  !---------------------------------
  program fred
  !---------------------------------
  real              :: dezz(1:10)
  real, allocatable :: jack(:)
  !
  allocate(jack(10)); jack = 9.
  dezz = john(jack,1)
  print*,'dezz = ',dezz

  contains
    !---------------------------------
    function john(t,il)
    !---------------------------------
    real  :: t(il:)
    real  :: john(1:10)
    john = 10.
    end function john
  end
