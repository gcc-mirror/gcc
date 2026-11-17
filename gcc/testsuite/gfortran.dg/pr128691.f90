! { dg-do compile }
!
! PR 128691

subroutine sub2(dd)
  character(len=4), target :: dd(:,:,:,:)
  if (size (dd(:,[1],:,:)) > 0) stop 1
end subroutine
