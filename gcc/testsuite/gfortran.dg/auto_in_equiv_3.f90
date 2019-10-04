! { dg-do run }
! { dg-options "-fdec-static -fno-automatic" }

! Contributed by Mark Eggleston <mark.eggleston@codethink.com>

! Storage is NOT on the static unless explicitly specified using the
! DEC extension "automatic". The address of the first local variable
! is used to determine that storage for the automatic local variable
! is different to that of a local variable with no attributes. The
! contents of the local variable in suba should be overwritten by the
! call to subb. 
!
program test
  integer :: dummy
  integer, parameter :: address = kind(loc(dummy))
  integer(address) :: ad1
  integer(address) :: ad2
  integer(address) :: ad3
  logical :: ok

  call suba(0, ad1)
  call subb(0, ad2)
  call suba(1, ad1)
  call subc(0, ad3)
  ok = (ad1.eq.ad3).and.(ad1.ne.ad2)
  if (.not.ok) stop 4

contains
  subroutine suba(option, addr) 
    integer, intent(in) :: option
    integer(address), intent(out) :: addr
    integer, automatic :: a
    integer :: b
    equivalence (a, b)
    addr = loc(a)
    if (option.eq.0) then
      ! initialise a and c
      a = 9
      if (a.ne.b) stop 1
      if (loc(a).ne.loc(b)) stop 2
    else
      ! a should've been overwritten
      if (a.eq.9) stop 3
    end if
  end subroutine suba

  subroutine subb(dummy, addr)
    integer, intent(in) :: dummy
    integer(address), intent(out) :: addr
    integer :: x
    addr = loc(x)
    x = 77
  end subroutine subb

  subroutine subc(dummy, addr)
    integer, intent(in) :: dummy
    integer(address), intent(out) :: addr
    integer, automatic :: y
    addr = loc(y)
    y = 77
  end subroutine subc

end program test
