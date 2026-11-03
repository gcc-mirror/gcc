! { dg-do run }
! { dg-options "-fdump-tree-original" }
! PR 82173
!
! Assigning a structure constructor with an allocatable component to an
! unlimited polymorphic variable leaked the constructor's component.
!
module mb
  type :: t
    real, allocatable :: v(:)
  end type
end module
program p
  use mb
  class(*), allocatable :: u
  u = t([1.,2.,3.])
  select type (u)
    type is (t)
      if (any (u%v /= [1.,2.,3.])) stop 1
    class default
      stop 2
  end select
  deallocate (u)
end program
! { dg-final { scan-tree-dump "__builtin_free \\(\\(void \\*\\) t\\.\[0-9\]+\\.v\\.data\\)" "original" } }
