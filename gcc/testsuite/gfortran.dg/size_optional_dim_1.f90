! { dg-do run }
! PR 30865 - passing a subroutine optional argument to size(dim=...)
! used to segfault.
program main
  implicit none
  integer :: a(2,3)
  integer :: ires

  call checkv (ires, a)
  if (ires /= 6) STOP 1
  call checkv (ires, a, 1)
  if (ires /= 2) STOP 2
contains
  subroutine checkv(ires,a1,opt1)
    integer, intent(out) :: ires
    integer :: a1(:,:)
    integer, optional :: opt1

    ires = size (a1, dim=opt1)
  end subroutine checkv
end program main
