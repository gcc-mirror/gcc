! { dg-do run }
!
! PR 51976: [F2003] Support deferred-length character components of derived types (allocatable string length)
!
! Contributed by Tobias Burnus  <burnus@gcc.gnu.org>

  type t
    character(len=:), allocatable :: str_comp
    character(len=:), allocatable :: str_comp1
  end type t
  type(t) :: x
  type(t), allocatable, dimension(:) :: array

  ! Check scalars
  allocate (x%str_comp, source = "abc")
  call check (x%str_comp, "abc")
  deallocate (x%str_comp)
  allocate (x%str_comp, source = "abcdefghijklmnop")
  call check (x%str_comp, "abcdefghijklmnop")
  x%str_comp = "xyz"
  call check (x%str_comp, "xyz")
  x%str_comp = "abcdefghijklmnop"
  x%str_comp1 = "lmnopqrst"
  call foo (x%str_comp1, "lmnopqrst")
  call bar (x, "abcdefghijklmnop", "lmnopqrst")

  ! Check arrays and structure constructors
  allocate (array(2), source = [t("abcedefg","hi"), t("jkl","mnop")])
  call check (array(1)%str_comp, "abcedefg")
  call check (array(1)%str_comp1, "hi")
  call check (array(2)%str_comp, "jkl")
  call check (array(2)%str_comp1, "mnop")
  deallocate (array)
  allocate (array(3), source = [x, x, x])
  array(2)%str_comp = "blooey"
  call bar (array(1), "abcdefghijklmnop", "lmnopqrst")
  call bar (array(2), "blooey", "lmnopqrst")
  call bar (array(3), "abcdefghijklmnop", "lmnopqrst")

contains

  subroutine foo (chr1, chr2)
    character (*) :: chr1, chr2
    call check (chr1, chr2)
  end subroutine

  subroutine bar (a, chr1, chr2)
    character (*) :: chr1, chr2
    type(t) :: a
    call check (a%str_comp, chr1)
    call check (a%str_comp1, chr2)
  end subroutine

  subroutine check (chr1, chr2)
    character (*) :: chr1, chr2
    if (len(chr1) .ne. len (chr2)) call abort
    if (chr1 .ne. chr2) call abort
  end subroutine

end
