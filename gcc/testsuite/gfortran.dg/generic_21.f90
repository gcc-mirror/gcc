! { dg-do compile }
!
! PR fortran/42858
!
! Contributed by Harald Anlauf
!
module gfcbug102
  implicit none
  type t_vector_segm
     real ,pointer :: x(:) => NULL()
  end type t_vector_segm

  type t_vector
     integer                       :: n_s     =  0
     type (t_vector_segm) ,pointer :: s (:)   => NULL()
  end type t_vector

  interface sqrt
     module procedure sqrt_vector
  end interface sqrt

contains
  function sqrt_vector (x) result (y)
    type (t_vector)             :: y
    type (t_vector) ,intent(in) :: x
    integer :: i
    do i = 1, y% n_s
       y% s(i)% x = sqrt (x% s(i)% x)
    end do
  end function sqrt_vector
end module gfcbug102

! { dg-final { cleanup-modules "gfcbug102" } }
