! { dg-do compile }
! Test the fix for PR52386.
!
! Contributed by Juergen Reuter  <reuter@physik.uni-freiburg.de>
!
module cascades
  implicit none
  private
contains
    function reduced (array)
      integer, dimension(:), allocatable :: reduced
      integer, dimension(:), intent(in) :: array
      logical, dimension(size(array)) :: mask
      mask = .true. 
      allocate (reduced (count (mask)))
      reduced = pack (array, mask)
    end function reduced
end module cascades
! { dg-final { cleanup-modules "cascades" } }

