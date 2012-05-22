! { dg-do run }
!
! PR fortran/53389
!
! The program was leaking memory before due to
! realloc on assignment and nested functions.
!
module foo
  implicit none
  contains

  function filler(array, val)
    real, dimension(:), intent(in):: array
    real, dimension(size(array)):: filler
    real, intent(in):: val

    filler=val

  end function filler
end module

program test
  use foo
  implicit none

  real, dimension(:), allocatable:: x, y
  integer, parameter:: N=1000 !*1000
  integer:: i

!  allocate( x(N) )
  allocate( y(N) )
  y=0.0

  do i=1, N
!    print *,i
    x=filler(filler(y, real(2*i)), real(i))
    y=y+x
  end do

end program test
