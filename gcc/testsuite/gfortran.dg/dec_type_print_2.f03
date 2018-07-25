! { dg-do run }
! { dg-options "-fdec -fcheck=all" }
!
! Verify that -fdec does not break parsing of PDTs.
! This test code is copied from pdt_1.f03 but compiled with -fdec.
!
program main
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

  type(mytype(b=4)) :: z(2)
  type(mytype(ftype, 4)) :: z2

  z(1)%i = 1
  z(2)%i = 2
  z(1)%d = reshape ([(real(i), i = 1, 16)],[4,4])
  z(2)%d = 10*z(1)%d
  z(1)%chr = "hello pdt"
  z(2)%chr = "goodbye pdt"

  z2%d = z(1)%d * 10 - 1
  z2%chr = "scalar pdt"

  call foo (z)
  call bar (z)
  call foobar (z2)
contains
  elemental subroutine foo (arg)
    type(mytype(8,*)), intent(in) :: arg
    if (arg%i .eq. 1) then
      if (trim (arg%chr) .ne. "hello pdt") error stop
      if (int (sum (arg%d)) .ne. 136) error stop
    else if (arg%i .eq. 2 ) then
      if (trim (arg%chr) .ne. "goodbye pdt") error stop
      if (int (sum (arg%d)) .ne. 1360) error stop
    else
      error stop
    end if
  end subroutine
  subroutine bar (arg)
    type(mytype(b=4)) :: arg(:)
    if (int (sum (arg(1)%d)) .ne. 136) call abort
    if (trim (arg(2)%chr) .ne. "goodbye pdt") call abort
  end subroutine
  subroutine foobar (arg)
    type(mytype(ftype, pdt_len)) :: arg
    if (int (sum (arg%d)) .ne. 1344) call abort
    if (trim (arg%chr) .ne. "scalar pdt") call abort
  end subroutine
end
