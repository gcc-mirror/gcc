! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! In the course of fixing PR83118, lots of issues came up with class array
! assignment, where temporaries are generated. This testcase checks that
! the use of assignment by allocate with source is OK, especially with array
! constructors using class arrays. While this test did run previously, the
! temporaries for such arrays were malformed with the class as the type and
! element lengths of 72 bytes rather than the 4 bytes of the decalred type.
!
! Contributed by Dominique d'Humieres  <dhumieres.dominique@free.fr>
!
type t1
   integer :: i = 5
end type t1
type, extends(t1) :: t2
   integer :: j = 6
end type t2

class(t1), allocatable :: a(:), b(:), c(:)
integer :: i

allocate(t2 :: a(3))
allocate(t2 :: b(5))
if (.not.check_t1 (a, [(5, i = 1, 3)], 2)) stop 1

allocate(c, source=[a, b ]) ! F2008, PR 44672
if (.not.check_t1 (c, [(5, i = 1, 8)], 1)) stop 2

deallocate(c)
allocate(c(8), source=[ a, b ])
if (.not.check_t1 (c, [(5, i = 1, 8)], 1)) stop 3

deallocate(c)
c = [t1 :: a, b ] ! F2008, PR 43366
if (.not.check_t1 (c, [(5, i = 1, 8)], 1)) stop 4
deallocate(a, b, c)

contains

  logical function check_t1 (arg, array, t)
    class(t1) :: arg(:)
    integer :: array (:), t
    check_t1 = .true.
    select type (arg)
    type is (t1)
      if (any (arg%i .ne. array)) check_t1 = .false.
      if (t .eq. 2) check_t1 = .false.
    type is (t2)
      if (any (arg%i .ne. array)) check_t1 = .false.
      if (t .eq. 1) check_t1 = .false.
    class default
      check_t1 = .false.
    end select
  end function check_t1

end
! { dg-final { scan-tree-dump-times "elem_len=72" 0 "original" } }
