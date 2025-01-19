module main
  use iso_c_binding, only: c_ptr
  implicit none
  contains
  
  subroutine f1 ()
    integer :: a, b, arr(10)
    real :: x
    complex :: c
    character :: ch
    logical :: bool
    type :: struct
      integer :: a
      real :: b
    end type
    type(struct) :: s
    type(c_ptr) :: p
    
    interface
    subroutine f0 (a, c, bool, s)
      import :: struct
      integer, intent(in) :: a
      complex, intent(out) :: c
      logical, intent(inout) :: bool
      type(struct) :: s
    end subroutine
    integer function f2 (arr, x, ch, b)
      integer, intent(inout) :: arr(:)
      real, intent(in) :: x
      character, intent(out) :: ch
      real :: b
    end function
    subroutine f3 (p)
      import :: c_ptr
      type(c_ptr) :: p
    end subroutine
    integer function f4 ()
    end function
    end interface

    !$omp dispatch
      b = f2(arr, x, ch, s%b)
    !$omp dispatch
      c = f2(arr(:5), x * 2.4, ch, s%b)
    !$omp dispatch
      arr(1) = f2(arr, x, ch, s%b)
    !$omp dispatch
      s%a = f2(arr, x, ch, s%b)
    !$omp dispatch
      x = f2(arr, x, ch, s%b)
    !$omp dispatch
      call f0(a, c, bool, s)
    !$omp dispatch
      call f0(f4(), c, bool, s)
      
    !$omp dispatch nocontext(.TRUE.)
      call f0(a, c, bool, s)
    !$omp dispatch nocontext(arr(2) < 10)
      call f0(a, c, bool, s)
    !$omp dispatch novariants(.FALSE.)
      call f0(a, c, bool, s)
    !$omp dispatch novariants(bool)
      call f0(a, c, bool, s)
    !$omp dispatch nowait
      call f0(a, c, bool, s)
    !$omp dispatch device(arr(9))
      call f0(a, c, bool, s)
    !$omp dispatch device(a + a)
      call f0(a, c, bool, s)
    !$omp dispatch device(-25373654)
      call f0(a, c, bool, s)
    !$omp dispatch is_device_ptr(p)
      call f3(p)
    !$omp dispatch depend(in: a, c, bool) depend(inout: s, arr(:3))
      call f0(a, c, bool, s)
  end subroutine
end module
