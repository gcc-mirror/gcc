! { dg-do run }
! { dg-options "-fcheck=pointer -fdump-tree-original" }
! PR100602 - Erroneous "pointer argument is not associated" runtime error

module m
  type :: T
  end type
contains
  subroutine f(this)
    class(T), intent(in)  :: this(:)
    class(T), allocatable :: ca(:)
    class(T), pointer     :: cp(:)
    if (size (this) == 0) return
    write(*,*) size (this)
    stop 1
    write(*,*) size (ca) ! Check #1
    write(*,*) size (cp) ! Check #2
  end subroutine f
end module

program main
  use m
  call f([T::])
end program

! { dg-final { scan-tree-dump-times "_gfortran_runtime_error_at" 2 "original" } }
! { dg-final { scan-tree-dump-times "Allocatable argument .*ca" 1 "original" } }
! { dg-final { scan-tree-dump-times "Pointer argument .*cp" 1 "original" } }
