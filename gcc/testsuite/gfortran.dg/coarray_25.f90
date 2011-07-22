! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! Used to be rejected with:
!  Error: Variable 'x' at (1) is a coarray or has a coarray
!  component and is not ALLOCATABLE, SAVE nor a dummy argument
!
! Is valid as "a" is allocatable, cf. C526
! and http://j3-fortran.org/pipermail/j3/2011-June/004403.html
!

  subroutine test2()
    type t
      integer, allocatable :: a(:)[:]
    end type t
    type(t) :: x
    allocate(x%a(1)[*])
  end subroutine test2
