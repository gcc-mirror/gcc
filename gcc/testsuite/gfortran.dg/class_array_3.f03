! { dg-do run }
!
! class based quick sort program - starting point comment #0 of pr41539
!
! Note assignment with vector index reference fails because temporary
! allocation does not occur - also false dependency detected. Nullification
! of temp descriptor data causes a segfault.
!
module m_qsort
 implicit none
 type, abstract :: sort_t
 contains
   procedure(disp), deferred :: disp
   procedure(lt_cmp), deferred :: lt_cmp
   procedure(assign), deferred :: assign
   generic :: operator(<) => lt_cmp
   generic :: assignment(=) => assign
 end type sort_t
 interface
   elemental integer function disp(a)
     import
     class(sort_t), intent(in) :: a
   end function disp
 end interface
 interface
   impure elemental logical function lt_cmp(a,b)
     import
     class(sort_t), intent(in) :: a, b
   end function lt_cmp
 end interface
 interface
   elemental subroutine assign(a,b)
     import
     class(sort_t), intent(out) :: a
     class(sort_t), intent(in) :: b
   end subroutine assign
 end interface
contains

 subroutine qsort(a)
   class(sort_t), intent(inout),allocatable :: a(:)
   class(sort_t), allocatable :: tmp (:)
   integer, allocatable :: index_array (:)
   integer :: i
   allocate (tmp(size (a, 1)), source = a)
   index_array = [(i, i = 1, size (a, 1))]
   call internal_qsort (tmp, index_array)   ! Do not move class elements around until end
   do i = 1, size (a, 1)                    ! Since they can be of arbitrary size.
     a(i) = tmp(index_array(i))             ! Vector index array would be neater
   end do
!    a = tmp(index_array)                    ! Like this - TODO: fixme
 end subroutine qsort

 recursive subroutine internal_qsort (x, iarray)
   class(sort_t), intent(inout),allocatable :: x(:)
   class(sort_t), allocatable :: ptr
   integer, allocatable :: iarray(:), above(:), below(:), itmp(:)
   integer :: pivot, nelem, i, iptr
   if (.not.allocated (iarray)) return
   nelem = size (iarray, 1)
   if (nelem .le. 1) return
   pivot = nelem / 2
   allocate (ptr, source = x(iarray(pivot))) ! Pointer to the pivot element
   do i = 1, nelem
     iptr = iarray(i)                  ! Index for i'th element
     if (ptr%lt_cmp (x(iptr))) then    ! Compare pivot with i'th element
       itmp = [iptr]
       above = concat (itmp, above)    ! Invert order to prevent infinite loops
     else
       itmp = [iptr]
       below = concat (itmp, below)    ! -ditto-
     end if
   end do
   call internal_qsort (x, above)      ! Recursive sort of 'above' and 'below'
   call internal_qsort (x, below)
   iarray = concat (below, above)      ! Concatenate the result
 end subroutine internal_qsort

 function concat (ia, ib) result (ic)
   integer, allocatable, dimension(:) :: ia, ib, ic
   if (allocated (ia) .and. allocated (ib)) then
     ic = [ia, ib]
   else if (allocated (ia)) then
     ic = ia
   else if (allocated (ib)) then
     ic = ib
   end if
 end function concat
end module m_qsort

module test
 use m_qsort
 implicit none
 type, extends(sort_t) :: sort_int_t
   integer :: i
 contains
   procedure :: disp => disp_int
   procedure :: lt_cmp => lt_cmp_int
   procedure :: assign => assign_int
 end type
contains
 elemental integer function disp_int(a)
     class(sort_int_t), intent(in) :: a
     disp_int = a%i
 end function disp_int
 elemental subroutine assign_int (a, b)
   class(sort_int_t), intent(out) :: a
   class(sort_t), intent(in) :: b         ! TODO: gfortran does not throw 'class(sort_int_t)'
   select type (b)
     class is (sort_int_t)
       a%i = b%i
     class default
       a%i = -1
   end select
 end subroutine assign_int
 impure elemental logical function lt_cmp_int(a,b) result(cmp)
   class(sort_int_t), intent(in) :: a
   class(sort_t), intent(in) :: b
   select type(b)
     type is(sort_int_t)
       if (a%i < b%i) then
         cmp = .true.
       else
         cmp = .false.
       end if
     class default
       ERROR STOP "Don't compare apples with oranges"
   end select
 end function lt_cmp_int
end module test

program main
 use test
 class(sort_t), allocatable :: A(:)
 integer :: i, m(5)= [7 , 4, 5, 2, 3]
 allocate (A(5), source = [(sort_int_t(m(i)), i=1,5)])
!  print *, "Before qsort: ", A%disp()
 call qsort(A)
!  print *, "After qsort:  ", A%disp()
 if (any (A%disp() .ne. [2,3,4,5,7])) call abort
end program main

! { dg-final { cleanup-modules "m_qsort test" } }
