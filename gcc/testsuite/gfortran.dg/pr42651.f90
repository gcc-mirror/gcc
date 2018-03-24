! { dg-do compile }
! PR fortran/42651
integer function func()
  asynchronous :: func
  integer, asynchronous:: b
  allocatable :: c
  volatile :: func
  type t
    sequence
    integer :: i = 5
  end type t
end function func

function func2() result(res) ! { dg-error " RESULT variable" }
  volatile res
  asynchronous res
  target func2            ! { dg-error " RESULT variable" }
  volatile func2          ! { dg-error " RESULT variable" }
  asynchronous func2      ! { dg-error " RESULT variable" }
  allocatable func2       ! { dg-error " RESULT variable" }
  dimension func2(2)      ! { dg-error " RESULT variable" }
  codimension func2[*]    ! { dg-error " RESULT variable" }
  contiguous func2        ! { dg-error " RESULT variable" }
end function func2 
