! { dg-do run }
!
! PR 51976: [F2003] Support deferred-length character components of derived types (allocatable string length)
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>

  type t
    character(len=:,kind=4), allocatable :: str_comp
    character(len=:,kind=4), allocatable :: str_comp1
  end type t
  type(t) :: x
  type(t), allocatable, dimension(:) :: array

  ! Check scalars
  allocate (x%str_comp, source = 4_"abc")
  call check (x%str_comp, 4_"abc")
  deallocate (x%str_comp)
  allocate (x%str_comp, source = 4_"abcdefghijklmnop")
  call check (x%str_comp, 4_"abcdefghijklmnop")
  x%str_comp = 4_"xyz"
  call check (x%str_comp, 4_"xyz")
  x%str_comp = 4_"abcdefghijklmnop"
  x%str_comp1 = 4_"lmnopqrst"
  call foo (x%str_comp1, 4_"lmnopqrst")
  call bar (x, 4_"abcdefghijklmnop", 4_"lmnopqrst")

  ! Check arrays and structure constructors
  allocate (array(2), source = [t(4_"abcedefg",4_"hi"), t(4_"jkl",4_"mnop")])
  call check (array(1)%str_comp, 4_"abcedefg")
  call check (array(1)%str_comp1, 4_"hi")
  call check (array(2)%str_comp, 4_"jkl")
  call check (array(2)%str_comp1, 4_"mnop")
  deallocate (array)
  allocate (array(3), source = [x, x, x])
  array(2)%str_comp = 4_"blooey"
  call bar (array(1), 4_"abcdefghijklmnop", 4_"lmnopqrst")
  call bar (array(2), 4_"blooey", 4_"lmnopqrst")
  call bar (array(3), 4_"abcdefghijklmnop", 4_"lmnopqrst")

contains

  subroutine foo (chr1, chr2)
    character (len=*,kind=4) :: chr1, chr2
    call check (chr1, chr2)
  end subroutine

  subroutine bar (a, chr1, chr2)
    character (len=*,kind=4) :: chr1, chr2
    type(t) :: a
    call check (a%str_comp, chr1)
    call check (a%str_comp1, chr2)
  end subroutine

  subroutine check (chr1, chr2)
    character (len=*,kind=4) :: chr1, chr2
    if (len(chr1) .ne. len (chr2)) call abort
    if (chr1 .ne. chr2) call abort
  end subroutine

end
