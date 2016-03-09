! { dg-do run }
!
! Test the fix for all the remaining issues in PR54070. These were all
! concerned with deferred length characters being returned as function results,
! except for comment #23 where the descriptor dtype was not correctly set and
! array IO failed in consequence.
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>
!
! The original comment #1 with an allocate statement.
! Allocatable, deferred length scalar resul.
function f()
  character(len=:),allocatable :: f
  allocate (f, source = "abc")
  f ="ABC"
end function
!
! Allocatable, deferred length, explicit, array result
function g(a) result (res)
  character(len=*) :: a(:)
  character(len (a)) :: b(size (a))
  character(len=:),allocatable :: res(:)
  integer :: i
  allocate (character(len(a)) :: res(2*size(a)))
  do i = 1, len (a)
    b(:)(i:i) = char (ichar (a(:)(i:i)) + 4)
  end do
  res = [a, b]
end function
!
! Allocatable, deferred length, array result
function h(a)
  character(len=*) :: a(:)
  character(len(a)) :: b (size(a))
  character(len=:),allocatable :: h(:)
  integer :: i
  allocate (character(len(a)) :: h(size(a)))
  do i = 1, len (a)
    b(:)(i:i) = char (ichar (a(:)(i:i)) + 32)
  end do
  h = b
end function

module deferred_length_char_array
contains
  function return_string(argument)
    character(*) :: argument
    character(:), dimension(:), allocatable :: return_string
    allocate (character (len(argument)) :: return_string(2))
    return_string = argument
  end function
end module

  use deferred_length_char_array
  character(len=3) :: chr(3)
  character(:), pointer :: s(:)
  character(6) :: buffer
  interface
    function f()
      character(len=:),allocatable :: f
    end function
    function g(a) result(res)
      character(len=*) :: a(:)
      character(len=:),allocatable :: res(:)
    end function
    function h(a)
      character(len=*) :: a(:)
      character(len=:),allocatable :: h(:)
    end function
  end interface

  if (f () .ne. "ABC") call abort
  if (any (g (["ab","cd"]) .ne. ["ab","cd","ef","gh"])) call abort
  chr = h (["ABC","DEF","GHI"])
  if (any (chr .ne. ["abc","def","ghi"])) call abort
  if (any (return_string ("abcdefg") .ne. ["abcdefg","abcdefg"])) call abort

! Comment #23
  allocate(character(3)::s(2))
  s(1) = 'foo'
  s(2) = 'bar'
  write (buffer, '(2A3)') s
  if (buffer .ne. 'foobar') call abort
end
