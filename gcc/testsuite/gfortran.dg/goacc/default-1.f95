! OpenACC default clause: valid syntax.

subroutine f1
  implicit none

  !$acc kernels default (none)
  !$acc end kernels
  !$acc parallel default (none)
  !$acc end parallel
  !$acc serial default (none)
  !$acc end serial

  !$acc kernels default (present)
  !$acc end kernels
  !$acc parallel default (present)
  !$acc end parallel
  !$acc serial default (present)
  !$acc end serial

end subroutine f1
