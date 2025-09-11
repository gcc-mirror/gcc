! { dg-do compile }
!
! This originally tested the fix for PR83611, in which the assignment caused a
! double free error and the initialization of 'foo' was not done. However, the
! initialization is not conforming (see PR84432 & PR114815) and so this test
! is now compile only and verifies the error detection. The program part has
! been deleted.
!
module pdt_m
  implicit none
  type :: vec(k)
     integer, len :: k=3
     integer :: foo(k)=[1,2,3]        ! { dg-error "not compatible with a default initializer" }
     character(len = k) :: chr = "ab" ! { dg-error "not compatible with a default initializer" }
  end type vec
end module pdt_m
