! { dg-do compile }
! Test by Dominique d'Humieres (PR 33957)
function bug(i) result(c)
  integer, pointer :: i
  character(len=merge(1,2, associated(i))) :: c
  c = ""
end function bug
