! { dg-do run }
! Tests the fixes for PR25597 and PR27096.
!
! This test combines the PR testcases.
!
  character(10), dimension (2) :: implicit_result
  character(10), dimension (2) :: explicit_result
  character(10), dimension (2) :: source
  source = "abcdefghij"
  explicit_result = join_1(source)
  if (any (explicit_result .ne. source)) STOP 1 

  implicit_result = reallocate_hnv (source, size(source, 1), LEN (source))
  if (any (implicit_result .ne. source)) STOP 2 

contains

! This function would cause an ICE in gfc_trans_deferred_array.
  function join_1(self) result(res)
    character(len=*), dimension(:) :: self
    character(len=len(self)), dimension(:), pointer :: res
    allocate (res(2))
    res = self
  end function

! This function originally ICEd and latterly caused a runtime error.
  FUNCTION reallocate_hnv(p, n, LEN)
    CHARACTER(LEN=LEN), DIMENSION(:), POINTER :: reallocate_hnv
    character(*), dimension(:) :: p
    ALLOCATE (reallocate_hnv(n))
    reallocate_hnv = p
  END FUNCTION reallocate_hnv

end


