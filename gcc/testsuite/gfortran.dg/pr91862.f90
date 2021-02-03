! { dg-do compile }
!
! Check that PR91862 remains fixed by patch for PRs 96100/101.
!
! Contributed by Gerhard Steinmetz  <>gscfq@t-online.de>
!
  call foo
  call bar
contains

  subroutine foo
    integer :: i
    character(3) :: a(3) = 'abc'
    character(1) :: b(1)
    b = [([ (a(1)(i:1), i=1,1) ])]
    print *, b
  end subroutine

  subroutine bar
    integer :: i
    character(3) :: a(3) = 'abc'
    character(1) :: b(1)
    b = [([ (a(1)(1:i), i=1,1) ])]
    print *, b
  end subroutine
end
