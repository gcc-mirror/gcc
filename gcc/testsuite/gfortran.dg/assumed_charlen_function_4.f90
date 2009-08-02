! { dg-do compile }
! { dg-options "-std=legacy" }
!
! Tests the fix for PR28600 in which the declaration for the
! character length n, would be given the DECL_CONTEXT of 'gee'
! thus causing an ICE.
!
! Contributed by Francois-Xavier Coudert  <fxcoudert@gcc.gnu.org>
!
subroutine bar(s, n)
 integer n
 character s*(n)
 character*3, dimension(:), pointer :: m
 s = ""
contains
 subroutine gee
    m(1) = s(1:3)
 end subroutine gee
end subroutine bar
