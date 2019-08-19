! { dg-run }
! { dg-options "-fdec-static" }

! Contributed by Mark Eggleston <mark.eggleston@codethink.com>

program test
  call suba(0)
  call subb(0)
  call suba(1)

contains
  subroutine suba(option) 
    integer, intent(in) :: option
    integer, automatic :: a
    integer :: b
    integer :: c
    equivalence (a, b)
    if (option.eq.0) then
      ! initialise a and c
      a = 9
      c = 99
      if (a.ne.b) stop 1
      if (loc(a).ne.loc(b)) stop 2
    else
      ! a should've been overwritten
      if (a.eq.9) stop 3
    end if
  end subroutine suba

  subroutine subb(dummy)
    integer, intent(in) :: dummy
    integer, automatic :: x
    integer :: y
    x = 77
    y = 7
  end subroutine subb

end program test
