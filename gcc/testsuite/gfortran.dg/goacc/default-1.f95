! OpenACC default clause: valid syntax.

subroutine f1
  implicit none

  !$acc kernels default (none)
  !$acc end kernels
  !$acc parallel default (none)
  !$acc end parallel

  !$acc kernels default (present)
  !$acc end kernels
  !$acc parallel default (present)
  !$acc end parallel
end subroutine f1
