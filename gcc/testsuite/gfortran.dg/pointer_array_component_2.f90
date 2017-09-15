! { dg-do run }
!
! Test the fix for PR34640. In the first version of the fix, the first
! testcase in PR51218 failed with a segfault. This test extracts the
! failing part and checks that all is well.
!
  type t_info_block
    integer                      :: n     =  0      ! number of elements
  end type t_info_block
  !
  type t_dec_info
    integer                      :: n     =  0      ! number of elements
    integer                      :: n_b   =  0      ! number of blocks
    type (t_info_block) ,pointer :: b (:) => NULL() ! info blocks
  end type t_dec_info
  !
  type t_vector_segm
    integer           :: n    =  0      ! number of elements
    real ,pointer :: x(:) => NULL() ! coefficients
  end type t_vector_segm
  !
  type t_vector
    type (t_dec_info)    ,pointer :: info    => NULL()  ! decomposition info
    integer                       :: n       =  0       ! number of elements
    integer                       :: n_s     =  0       ! number of segments
    integer                       :: alloc_l =  0       ! allocation level
    type (t_vector_segm) ,pointer :: s (:)   => NULL()  ! vector blocks
  end type t_vector


  type(t_vector) :: z
  type(t_vector_segm), pointer :: ss

  allocate (z%s(2))
  do i = 1, 2
    ss => z%s(i)
    allocate (ss%x(2), source = [1.0, 2.0]*real(i))
  end do

! These lines would segfault.
  if (int (sum (z%s(1)%x)) .ne. 3) call abort
  if (int (sum (z%s(1)%x * z%s(2)%x)) .ne. 10) call abort
end
