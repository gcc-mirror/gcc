! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
   type t
  end type t
  type(t) :: a[*]
  call test(a) ! { dg-error "Rank mismatch in argument 'x' at .1. .rank-1 and scalar." }
contains
  subroutine test(x)
   class(t) :: x(:)[*]
   print *, ucobound(x)
  end
end
