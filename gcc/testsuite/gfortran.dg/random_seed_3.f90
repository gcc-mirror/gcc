! { dg-do run }
! Check that array constructors using non-compile-time
! iterators are handled correctly.
program main
  implicit none
  call init_random_seed
contains
  SUBROUTINE init_random_seed()
    INTEGER :: i, n, clock
    INTEGER, DIMENSION(:), ALLOCATABLE :: seed
  
    CALL RANDOM_SEED(size = n)
    ALLOCATE(seed(n))

    CALL SYSTEM_CLOCK(COUNT=clock)
    
    seed = clock + 37 * (/ (i - 1, i = 1, n) /)
    CALL RANDOM_SEED(PUT = seed)
  
    DEALLOCATE(seed)
  END SUBROUTINE init_random_seed
end program main
