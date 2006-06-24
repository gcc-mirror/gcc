! { dg-do run }
! Test fix of PR28118, in which a substring reference to an
! actual argument with an array reference would cause a segfault.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
program gfcbug33
  character(12) :: a(2)
  a(1) = "abcdefghijkl"
  a(2) = "mnopqrstuvwx"
  call foo ((a(2:1:-1)(6:)))
  call bar ((a(:)(7:11)))
contains
  subroutine foo (chr)
    character(7) :: chr(:)
    if (chr(1)//chr(2) .ne. "rstuvwxfghijkl") call abort ()
  end subroutine foo
  subroutine bar (chr)
    character(*) :: chr(:)
    if (trim(chr(1))//trim(chr(2)) .ne. "ghijkstuvw") call abort ()
  end subroutine bar
end program gfcbug33
