! { dg-do run }
! Testcase for PR libfortran/31001
  implicit none

  integer :: i, j, k
  integer, allocatable :: mm(:)
  logical, allocatable :: mask(:)

  do i = 2, -2, -1
    do k = 0, 1
      allocate (mm(i), mask(i))
      mm(:) = k
      mask(:) = (mm == 0)
      j = count (mask)
      print *, pack (mm, mask)
      if (size (pack (mm, mask)) /= j) call abort
      deallocate (mm, mask)
    end do
  end do
end
