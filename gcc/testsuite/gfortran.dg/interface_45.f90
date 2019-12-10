! { dg-do compile }
! PR 92863 - this used to ICE
! Test case by Arseny Solokha.

type(l1) function mp() ! { dg-error "type for function" }
  call sub(mp) ! { dg-error "Type mismatch" }
end function mp

function bi(ry)
  call sub(ry) ! { dg-error "Type mismatch" }
end function bi
