module main
  use iso_c_binding, only: c_funptr
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
    type(c_funptr) :: p
    
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
    end interface
    procedure(f0), pointer:: fp => NULL()

    !$omp dispatch              !{ dg-error "'OMP DISPATCH' directive at .1. must be followed by a procedure call with optional assignment" }
50    b = f2(arr, x, ch, s%b) + a
    !$omp dispatch              !{ dg-error "'OMP DISPATCH' directive at .1. must be followed by a procedure call with optional assignment" }
      a = b
    !$omp dispatch              !{ dg-error "'OMP DISPATCH' directive at .1. must be followed by a procedure call with optional assignment" }
      b = Not (2)
    !$omp dispatch
    !$omp threadprivate(a)	!{ dg-error "'OMP DISPATCH' directive must be followed by a procedure call with optional assignment at .1." } 
      a = f2(arr, x, ch, s%b)
    !$omp dispatch
      print *, 'This is not allowed here.'  !{ dg-error "'OMP DISPATCH' directive must be followed by a procedure call with optional assignment at .1." } 
    !$omp dispatch
      goto 50                   !{ dg-error "'OMP DISPATCH' directive must be followed by a procedure call with optional assignment at .1." } 
    !$omp dispatch              !{ dg-error "'OMP DISPATCH' directive at .1. cannot be followed by a procedure pointer" }
      call fp(a, c, bool, s)
      
    !$omp dispatch nocontext(s) !{ dg-error "NOCONTEXT clause at .1. requires a scalar LOGICAL expression" } 
      call f0(a, c, bool, s)
    !$omp dispatch nocontext(a, b) !{ dg-error "Invalid expression after 'nocontext.' at .1." } 
      call f0(a, c, bool, s)
    !$omp dispatch nocontext(a) nocontext(b) !{ dg-error "Duplicated 'nocontext' clause at .1." } 
      call f0(a, c, bool, s)
    !$omp dispatch novariants(s) !{ dg-error "NOVARIANTS clause at .1. requires a scalar LOGICAL expression" } 
      call f0(a, c, bool, s)
    !$omp dispatch novariants(a, b) !{ dg-error "Invalid expression after 'novariants.' at .1." } 
      call f0(a, c, bool, s)
    !$omp dispatch novariants(a) novariants(b) !{ dg-error "Duplicated 'novariants' clause at .1." } 
      call f0(a, c, bool, s)
    !$omp dispatch nowait nowait !{ dg-error "Duplicated 'nowait' clause at .1." } 
      call f0(a, c, bool, s)
    !$omp dispatch device(x) !{ dg-error "DEVICE clause at .1. requires a scalar INTEGER expression" } 
      call f0(a, c, bool, s)
    !$omp dispatch device(arr) !{ dg-error "DEVICE clause at .1. requires a scalar INTEGER expression" } 
      call f0(a, c, bool, s)
    !$omp dispatch is_device_ptr(x) !{ dg-error "List item 'x' in IS_DEVICE_PTR clause at .1. must be of TYPE.C_PTR." } 
      call f0(a, c, bool, s)
    !$omp dispatch is_device_ptr(arr) !{ dg-error "List item 'arr' in IS_DEVICE_PTR clause at .1. must be of TYPE.C_PTR." } 
      call f0(a, c, bool, s)
    !$omp dispatch is_device_ptr(p) !{ dg-error "List item 'p' in IS_DEVICE_PTR clause at .1. must be of TYPE.C_PTR." } 
      call f0(a, c, bool, s)
    !$omp dispatch depend(inout: f0) !{ dg-error "Object 'f0' is not a variable at .1." } 
      call f0(a, c, bool, s)
  end subroutine
end module
