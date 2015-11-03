! { dg-do compile }
!
! PR fortran/65889
!
!
module m
  type n
  end type n
contains
  subroutine g(ns)
    class(n), intent(out), allocatable, dimension(:) :: ns
    class(n), allocatable, dimension(:) :: tmp
    write (0,*) sizeof(ns), sizeof(tmp)
  end subroutine g
end module m
