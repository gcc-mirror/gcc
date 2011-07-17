! { dg-do compile }
! Test the patch for PR36374 in which the different
! symbols for 'foobar' would be incorrectly flagged as
! ambiguous in foo_mod.
!
! Contributed by Salvatore Filippone  <sfilippone@uniroma2.it>
!
module s_foo_mod
  type s_foo_type
    real(kind(1.e0)) :: v
  end type s_foo_type
  interface foobar
    subroutine s_foobar(x)
      import 
      type(s_foo_type), intent (inout) :: x
    end subroutine s_foobar
  end interface
end module s_foo_mod

module d_foo_mod
  type d_foo_type
    real(kind(1.d0)) :: v
  end type d_foo_type
  interface foobar
    subroutine d_foobar(x)
      import  
      type(d_foo_type), intent (inout) :: x
    end subroutine d_foobar
  end interface
end module d_foo_mod

module foo_mod
  use s_foo_mod
  use d_foo_mod
end module foo_mod

subroutine s_foobar2(x)  
  use foo_mod
end subroutine s_foobar2
! { dg-final { cleanup-modules "s_foo_mod d_foo_mod foo_mod" } }
