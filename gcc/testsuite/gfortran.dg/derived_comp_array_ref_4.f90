! { dg-do run }
! Tests the fix for PR33376, which was a regression caused by the
! fix for PR31564.
!
! Contributed by Harald Anlauf <anlauf@gmx.de>
!
module foo
  implicit none
  public chk

  type mytype
    character(len=4) :: str
  end type mytype
  type (mytype) ,parameter :: chk (2) &
                      = (/ mytype ("abcd") , mytype ("efgh") /)
end module foo

module gfcbug70
  use foo, only: chk_ => chk
  implicit none
contains

  subroutine chk (i)
    integer, intent(in) :: i
    if (i .eq. 1) then
      if (chk_(i)% str .ne. "abcd") call abort ()
    else
      if (chk_(i)% str .ne. "efgh") call abort ()
    end if

  end subroutine chk
end module gfcbug70

  use gfcbug70
  call chk (2)
  call chk (1)
end
! { dg-final { cleanup-modules "foo gfcbug70" } }
