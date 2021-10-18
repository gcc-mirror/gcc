! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/91863
!
! Contributed by G. Steinmetz
!

subroutine sub(x) bind(c)
  implicit none (type, external)
  integer, allocatable, intent(out) :: x(:)

  allocate(x(3:5))
  x(:) = [1, 2, 3]
end subroutine sub


program p
  implicit none (type, external)
  interface
    subroutine sub(x) bind(c)
      integer, allocatable, intent(out) :: x(:)
    end
  end interface
  integer, allocatable :: a(:)

  call sub(a)
  if (.not.allocated(a)) stop 1
  if (any(shape(a) /= [3])) stop 2
  if (lbound(a,1) /= 3 .or. ubound(a,1) /= 5) stop 3
  print *, a(0), a(1), a(2), a(3), a(4)
  print *, a
  if (any(a /= [1, 2, 3])) stop 4
end program p

! "cfi" only appears in context of "a" -> bind-C descriptor
! the intent(out) implies freeing in the callee (!) (when implemented in Fortran), hence the "free"
! and also in the caller (when implemented in Fortran)
! It is the only 'free' as 'a' is part of the main program and, hence, implicitly has the SAVE attribute.
! The  'cfi = 0' appears before the call due to the deallocate and when preparing the C descriptor
! As cfi (i.e. the descriptor itself) is allocated in libgomp, it has to be freed after the call.

! { dg-final { scan-tree-dump-times "__builtin_free" 2 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free \\(_x->base_addr\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "_x->base_addr = 0B;" 1 "original" } }
! { dg-final { scan-tree-dump-times "__builtin_free \\(cfi\\.\[0-9\]+\\.base_addr\\);" 1 "original" } }
! { dg-final { scan-tree-dump-times "cfi\\.\[0-9\]+\\.base_addr = 0B;" 1 "original" } }
