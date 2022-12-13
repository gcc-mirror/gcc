! { dg-do run }
! PR fortran/108010 - ICE in reduce_unary, reduce_binary_aa
! Contributed by G.Steinmetz

program p
  implicit none
  print *,   + [integer :: [real ::]]
  print *,   - [integer :: [real ::]]
  print *, 1 + [integer :: [real ::]]
  print *, 1 - [integer :: [real ::]]
  print *, 2 * [integer :: [real ::]]
  print *,   - [real :: [real ::], 2]
  print *,   + [integer :: [real ::], 2]
  print *,   - [integer :: [real ::], 2]
  print *, 1 + [integer :: [real ::], 2]
  print *, 1 - [integer :: [real ::], 2]
  print *, 2 * [integer :: [real ::], 2]
  print *, [integer :: [real ::]] + [integer :: [real ::]]
  print *, [integer :: [real ::]] - [integer :: [real ::]]
  print *, [integer :: [real ::]] * [integer :: [real ::]]
  print *, [integer :: [real ::], 2] + [real :: [real ::], 3]
  print *, [integer :: [real ::], 2] - [real :: [real ::], 3]
  print *, [integer :: [real ::], 2] * [real :: [real ::], 3]

  ! Validate type of resulting arrays
  if (.not. is_int ([integer :: [real ::]]                         )) stop 1
  if (.not. is_int ([integer :: [real ::]] + [integer :: [real ::]])) stop 2
  if (.not. is_real([real :: [integer ::]]                         )) stop 3
  if (.not. is_real([real :: [integer ::]] + [real :: [integer ::]])) stop 4
  if (.not. is_real([real :: [integer ::]] + [integer :: [real ::]])) stop 5
  if (.not. is_real([integer :: [real ::]] + [real :: [integer ::]])) stop 6

contains

  logical function is_int (x)
    class(*) :: x(:)
    select type (x)
    type is (integer)
       is_int = .true.
    class default
       is_int = .false.
    end select
  end function is_int
    
  logical function is_real (x)
    class(*) :: x(:)
    select type (x)
    type is (real)
       is_real = .true.
    class default
       is_real = .false.
    end select
  end function is_real
end
