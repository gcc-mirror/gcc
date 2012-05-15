! { dg-do compile }
!
! PR fortran/38095
!
! Contributed by Vivek Rao
!
! Compiling the program below gave an ICE
!
module bar
  implicit none
contains
elemental function trim_append(xx,yy) result(xy)
  character (len=*), intent(in) :: xx,yy
  character (len=len(xx) + len(yy)) :: xy
  xy = trim(xx) // yy
end function trim_append
function same(xx) result(yy)
  character (len=*), intent(in) :: xx(:)
  character (len=len(xx))       :: yy(size(xx))
  yy = [xx]
end function same
subroutine foo(labels)
  character (len=*), intent(in) :: labels(:)
  print*,"size(labels)=",size(labels)
end subroutine foo
subroutine xmain()
  call foo(trim_append(["a"],same(["b"])))
end subroutine xmain
end module bar

program main
  use bar
  call xmain()
end program main
