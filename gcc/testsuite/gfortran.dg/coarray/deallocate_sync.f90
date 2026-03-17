! { dg-do run }
! Test case from pr121429 which failed because the deallocate was not
! doing an implicit sync all.
program main
  implicit none
  integer, allocatable, codimension[:] :: a(:), b(:)
  integer :: n, i, left, right
  integer :: k
  n = num_images()
  i = this_image()
  !   We skip this test for -fcoarray=single
  if (n .ne. 1) then
    ! Verify in the testsuite that num_images = 8 as set in caf.exp
    if (n .ne. 8) error stop "Need at least three images"
    left = modulo(i+n-2,n)+1
    right = modulo(i,n)+1
    do k=1,1000
       allocate(a(k+5)[*])
       allocate(b(k)[*])
       b(:)[right] = right + 1000 * k
       deallocate (a)  ! This synchronizes automatically
       if (any(b(:)[left] /= left + 1000 * k)) error stop "Test failed"
       deallocate (b)
    end do
  end if
end program main
