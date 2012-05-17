! { dg-do compile }
! { dg-options "-fcoarray=single" }
!
! PR fortran/18918
!

! Was failing before as the "x%a()[]" was
! regarded as coindexed
subroutine test2()
  type t
    integer, allocatable :: a(:)[:]
  end type t
  type(t), SAVE :: x
  allocate(x%a(1)[*])
end subroutine test2


module m
  integer, allocatable :: a(:)[:]
end module m

! Was failing as "a" was allocatable but
! as->cotype was not AS_DEFERERED.
use m
end
