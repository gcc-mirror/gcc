! { dg-do run }
! { dg-options "-fcheck=all" }
! { dg-shouldfail "value of the PDT LEN parameter" }
!
! Reduced version of pdt_1.f03 to check that an incorrect
! value for the parameter 'b' in the dummy is picked up.
!
  implicit none
  integer, parameter :: ftype = kind(0.0e0)
  integer :: pdt_len = 4
  integer :: i
  type :: mytype (a,b)
    integer, kind :: a = kind(0.0d0)
    integer, LEN :: b
    integer :: i
    real(kind = a) :: d(b, b)
    character (len = b*b) :: chr
  end type

  type(mytype(ftype, pdt_len)) :: z2
  call foobar (z2)
contains
  subroutine foobar (arg)
    type(mytype(ftype, 8)) :: arg
    print *, arg%i
  end subroutine
end
