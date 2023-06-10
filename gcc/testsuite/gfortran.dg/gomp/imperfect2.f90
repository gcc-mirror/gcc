! This test case is expected to fail due to errors.

! Note that the calls to these functions in the test case don't make
! any sense in terms of behavior, they're just there to test the error
! behavior.

module omp_lib
 use iso_c_binding
  interface
     integer function omp_get_thread_num ()
     end
     subroutine omp_set_max_levels (i)
       integer :: i
     end
  end interface
end module

program junk
  use omp_lib
  implicit none
  
contains
	 
subroutine f1 (depth, iter)
  integer :: depth, iter
end subroutine

subroutine f2 (depth, iter)
  integer :: depth, iter
end subroutine

subroutine s1 (a1, a2, a3)
  integer :: a1, a2, a3
  integer :: i, j, k

  integer :: m

  !$omp do collapse(3) 
  do i = 1, a1
    call f1 (1, i)
    m = omp_get_thread_num ()  ! { dg-error "OpenMP API call in intervening code" }
    do j = 1, a2 + omp_get_thread_num ()  ! This is OK
      call f1 (2, j)
      do k = 1, a3
        call f1 (m, k)
	call omp_set_max_active_levels (k)  ! This is OK too
        call f2 (m, k)
      end do
      call f2 (2, j)
    call omp_set_max_active_levels (i)  ! { dg-error "OpenMP API call in intervening code" }
    end do
    call f2 (1, i)
  end do
end subroutine

end program
