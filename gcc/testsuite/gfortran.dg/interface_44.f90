! { dg-do compile }
! PR 92964 - this used to ICE.
! Original test case by Arseny Solokha
type(e6) function dn() ! { dg-error "The type for function" }
  call sub(dn)
end function dn
