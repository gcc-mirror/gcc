! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Test for polymorphic coarrays
!
subroutine s2()
  type t
  end type t
  class(t) :: A(:)[4,2:*] ! { dg-error "is not ALLOCATABLE, SAVE nor a dummy argument" }
  print *, ucobound(a)
  allocate(a) ! { dg-error "must be ALLOCATABLE or a POINTER" }
end

