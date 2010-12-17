! { dg-do compile }

! PR fortran/45783
! PR fortran/45795
! This used to fail because of incorrect compile-time typespec on the
! SELECT TYPE selector.

! This is the test-case from PR 45795.
! Contributed by Salvatore Filippone, sfilippone@uniroma2.it.

module base_mod
  
  type  :: base
    integer     :: m, n
  end type base

end module base_mod

module s_base_mod
  
  use base_mod

  type, extends(base) :: s_base
  contains
    procedure, pass(a) :: cp_to_foo   => s_base_cp_to_foo   
    
  end type s_base
  
  
  type, extends(s_base) :: s_foo
    
    integer              :: nnz
    integer, allocatable :: ia(:), ja(:)
    real, allocatable :: val(:)
    
  contains
    
    procedure, pass(a) :: cp_to_foo    => s_cp_foo_to_foo
    
  end type s_foo
  
  
  interface 
    subroutine s_base_cp_to_foo(a,b,info) 
      import :: s_base, s_foo
      class(s_base), intent(in) :: a
      class(s_foo), intent(inout) :: b
      integer, intent(out)            :: info
    end subroutine s_base_cp_to_foo
  end interface
  
  interface 
    subroutine s_cp_foo_to_foo(a,b,info) 
      import :: s_foo
      class(s_foo), intent(in) :: a
      class(s_foo), intent(inout) :: b
      integer, intent(out)            :: info
    end subroutine s_cp_foo_to_foo
  end interface

end module s_base_mod


subroutine trans2(a,b)
  use s_base_mod
  implicit none 

  class(s_base), intent(out) :: a
  class(base), intent(in)   :: b

  type(s_foo) :: tmp
  integer err_act, info


  info = 0
  select type(b)
  class is (s_base)
    call b%cp_to_foo(tmp,info)
  class default
    info = -1
    write(*,*) 'Invalid dynamic type'
  end select
  
  if (info /= 0) write(*,*) 'Error code ',info

  return

end subroutine trans2

! { dg-final { cleanup-modules "base_mod s_base_mod" } }
