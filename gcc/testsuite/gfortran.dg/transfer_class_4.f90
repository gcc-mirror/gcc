! { dg-do run }
!
! Fix TRANSFER intrinsic for unlimited polymorphic SOURCEs - PR98534
! Note that unlimited polymorphic MOLD is a TODO.
!
! Contributed by Paul Thomas  <pault@gcc.gnu.org>
!
  use, intrinsic :: ISO_FORTRAN_ENV, only: real32
  implicit none
  character(*), parameter :: string = "abcdefgh"
  character(len=:), allocatable :: string_a(:)
  class(*), allocatable :: star
  class(*), allocatable :: star_a(:)
  character(len=:), allocatable :: chr
  character(len=:), allocatable :: chr_a(:)
  integer :: sz, sum1, sum2, i
  real(real32) :: r = 1.0

! Part 1: worked correctly
  star = r
  sz = storage_size (star)/8
  allocate (character(len=sz) :: chr)
  chr = transfer (star, chr)
  sum1 = sum ([(ichar(chr(i:i)), i = 1, sz)])
  chr = transfer(1.0, chr)
  sum2 = sum ([(ichar(chr(i:i)), i = 1, sz)])

  if (sz /= storage_size (r)/8) stop 1
  if (sum1 /= sum2) stop 2

  deallocate (star) ! The automatic reallocation causes invalid writes
                    ! and memory leaks. Even with this deallocation
                    ! The invalid writes still occur.
  deallocate (chr)

! Part 2: Got everything wrong because '_len' field of unlimited polymorphic
! expressions was not used.
  star = string
  sz = storage_size (star)/8
  if (sz /= len (string)) stop 3 ! storage_size failed

  sz = len (string) ! Ignore previous error in storage_size
  allocate (character(len=sz) :: chr)
  chr = transfer (star, chr)
  sum1 = sum ([(ichar(chr(i:i)), i = 1, sz)])
  chr = transfer(string, chr)
  sum2 = sum ([(ichar(chr(i:i)), i = 1, sz)])
  if (sum1 /= sum2) stop 4       ! transfer failed

! Check that arrays are OK for transfer
  star_a = ['abcde','fghij']
  allocate (character (len = 5) :: chr_a(2))
  chr_a = transfer (star_a, chr_a)
  if (any (chr_a .ne. ['abcde','fghij'])) stop 5

! Check that string length and size are correctly handled
  string_a = ["abcdefgh", "ijklmnop"]
  star_a = string_a;
  chr_a = transfer (star_a, chr_a) ! Old string length used for size
  if (size(chr_a) .ne. 4) stop 6
  if (len(chr_a) .ne. 5) stop 7
  if (trim (chr_a(3)) .ne. "klmno") stop 8
  if (chr_a(4)(1:1) .ne. "p") stop 9

  chr_a = transfer (star_a, string_a) ! Use correct string_length for payload
  if (size(chr_a) .ne. 2) stop 10
  if (len(chr_a) .ne. 8) stop 11
  if (any (chr_a .ne. string_a)) stop 12

! Check that an unlimited polymorphic function result is transferred OK
  deallocate (chr_a)
  string_a = ['abc', 'def', 'hij']
  chr_a = transfer (foo (string_a), string_a)
  if (any (chr_a .ne. string_a)) stop 13

! Finally, check that the SIZE gives correct results with unlimited sources.
  chr_a = transfer (star_a, chr_a, 4)
  if (chr_a (4) .ne. 'jkl') stop 14

  deallocate (star, chr, star_a, chr_a, string_a)
contains
  function foo (arg) result(res)
    character(*), intent(in) :: arg(:)
    class(*), allocatable :: res(:)
    res = arg
  end
end
