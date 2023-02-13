! { dg-do compile }
! { dg-options "-O2 -fno-automatic" }
!
! PR fortran/95107 - do not make associate variables TREE_STATIC
! Contributed by G.Steinmetz

program p
  type t
     real, pointer :: a => null()
  end type
  type t2
     type(t) :: b(1)
  end type
  type(t2), save :: x
  associate (y => x%b)
  end associate
end
