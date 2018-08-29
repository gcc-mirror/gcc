
module amod

contains

subroutine asubr (b)
  implicit none
  integer :: b(8)

  !$acc declare copy (b) ! { dg-error "Invalid clause in module" }
  !$acc declare copyout (b) ! { dg-error "Invalid clause in module" }
  !$acc declare present (b) ! { dg-error "Invalid clause in module" }
  !$acc declare present_or_copy (b) ! { dg-error "Invalid clause in module" }
  !$acc declare present_or_copyin (b) ! { dg-error "present on multiple" }
  !$acc declare present_or_copyout (b) ! { dg-error "Invalid clause in module" }
  !$acc declare present_or_create (b) ! { dg-error "present on multiple" }
  !$acc declare deviceptr (b) ! { dg-error "Invalid clause in module" }
  !$acc declare create (b) copyin (b) ! { dg-error "present on multiple" }

end subroutine

end module

module bmod

  implicit none
  integer :: a, b, c, d, e, f, g, h, i
  common /data1/ a, b, c
  common /data2/ d, e, f
  common /data3/ g, h, i
  !$acc declare link (a) ! { dg-error "element of a COMMON" }
  !$acc declare link (/data1/)
  !$acc declare link (a, b, c) ! { dg-error "element of a COMMON" }
  !$acc declare link (/foo/) ! { dg-error "not found" }
  !$acc declare device_resident (/data2/)
  !$acc declare device_resident (/data3/) ! { dg-error "present on multiple clauses" }
  !$acc declare device_resident (g, h, i)

end module

subroutine bsubr (foo)
  implicit none

  integer, dimension (:) :: foo

  !$acc declare copy (foo) ! { dg-error "Assumed-size dummy array" }
  !$acc declare copy (foo(1:2)) ! { dg-error "Assumed-size dummy array" }

end subroutine bsubr

subroutine multiline
  integer :: b(8)

  !$acc declare copyin (b) ! { dg-error "present on multiple clauses" }
  !$acc declare copyin (b)

end subroutine multiline

subroutine subarray
  integer :: c(8)

  !$acc declare copy (c(1:2)) ! { dg-error "Array sections: 'c' not allowed" }

end subroutine subarray

program test
  integer :: a(8)

  !$acc declare create (a) copyin (a) ! { dg-error "present on multiple clauses" }

end program
