! { dg-do compile }
! Test fix of PR28118, in which a substring reference to an
! actual argument with an array reference would cause a segfault.
!
! Revised for PR fortran/83522
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
program gfcbug33
  character(12) :: a(2)
  a(1) = "abcdefghijkl"
  a(2) = "mnopqrstuvwx"
  call foo ((a(2:1:-1)(6:))) ! { dg-error "Substring reference of nonscalar not permitted" }
  call bar ((a(:)(7:11))) ! { dg-error "Substring reference of nonscalar not permitted" }
contains
  subroutine foo (chr)
    character(7) :: chr(:)
    if (chr(1)//chr(2) .ne. "rstuvwxfghijkl") STOP 1
  end subroutine foo
  subroutine bar (chr)
    character(*) :: chr(:)
    if (trim(chr(1))//trim(chr(2)) .ne. "ghijkstuvw") STOP 2
  end subroutine bar
end program gfcbug33
