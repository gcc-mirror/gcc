! { dg-do run }
! Tests the fix for PR36433 in which a check for the array size
! or character length of the actual arguments of foo and bar
! would reject this legal code.
!
! Contributed by Paul Thomas <pault@gcc.gnu.org>
!
module m
contains
  function proc4 (arg, chr)
    integer, dimension(10) :: proc4
    integer, intent(in) :: arg
    character(8), intent(inout) :: chr
    proc4 = arg
    chr = "proc4"
  end function
  function chr_proc ()
    character(8) :: chr_proc
    chr_proc = "chr_proc"
  end function
end module

program procPtrTest
  use m
  character(8) :: chr
  interface
    function proc_ext (arg, chr)
      integer, dimension(10) :: proc_ext
      integer, intent(in) :: arg
      character(8), intent(inout) :: chr
    end function
  end interface
! Check the passing of a module function
  call foo (proc4, chr)
  if (trim (chr) .ne. "proc4") STOP 1
! Check the passing of an external function
  call foo (proc_ext, chr)
! Check the passing of a character function
  if (trim (chr) .ne. "proc_ext") STOP 2
  call bar (chr_proc)
contains
  subroutine foo (p, chr)
    character(8), intent(inout) :: chr
    integer :: i(10)
    interface
      function p (arg, chr)
        integer, dimension(10) :: p
        integer, intent(in) :: arg
        character(8), intent(inout) :: chr
      end function
    end interface
    i = p (99, chr)
    if (any(i .ne. 99)) STOP 3
  end subroutine
  subroutine bar (p)
    interface
      function p ()
        character(8):: p
      end function
    end interface
    if (p () .ne. "chr_proc") STOP 4
  end subroutine
end program 

function proc_ext (arg, chr)
  integer, dimension(10) :: proc_ext
  integer, intent(in) :: arg
  character(8), intent(inout) :: chr
  proc_ext = arg
  chr = "proc_ext"
end function
