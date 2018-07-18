! { dg-do run }
! Tests fix for PR31193, in which the character length for MOLD in
! case 1 below was not being translated correctly for character
! constants and an ICE ensued.  The further cases are either checks
! or new bugs that were found in the course of development cases 3 & 5.
!
! Contributed by Brooks Moses <brooks@gcc.gnu.org>
!
function NumOccurances (string, chr, isel) result(n)
  character(*),intent(in) :: string
  character(1),intent(in) :: chr
  integer :: isel
!
! return number of occurances of character in given string
!
    select case (isel)
      case (1)
      n=count(transfer(string, char(1), len(string))==chr)
      case (2)
      n=count(transfer(string, chr, len(string))==chr)
      case (3)
      n=count(transfer(string, "a", len(string))==chr)
      case (4)
      n=count(transfer(string, (/"a","b"/), len(string))==chr)
      case (5)
      n=count(transfer(string, string(1:1), len(string))==chr)
    end select
  return
end

  if (NumOccurances("abacadae", "a", 1) .ne. 4) STOP 1
  if (NumOccurances("abacadae", "a", 2) .ne. 4) STOP 2
  if (NumOccurances("abacadae", "a", 3) .ne. 4) STOP 3
  if (NumOccurances("abacadae", "a", 4) .ne. 4) STOP 4
  if (NumOccurances("abacadae", "a", 5) .ne. 4) STOP 5
end
