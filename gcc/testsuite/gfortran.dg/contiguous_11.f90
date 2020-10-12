! { dg-do compile }
!
! PR fortran/97242
!
implicit none
type t
  integer, allocatable :: A(:,:,:)
  integer :: D(5,5,5)
end type t

type(t), target :: B(5)
integer, pointer, contiguous :: P(:,:,:)
integer, target :: C(5,5,5)
integer :: i

i = 1

! OK: contiguous
P => B(i)%A
P => B(i)%A(:,:,:)
P => C
P => C(:,:,:)
call foo (B(i)%A)
call foo (B(i)%A(:,:,:))
call foo (C)
call foo (C(:,:,:))

! Invalid - not contiguous
! "If the pointer object has the CONTIGUOUS attribute, the pointer target shall be contiguous."
! → known to be noncontigous (not always checkable, however)
P => B(i)%A(:,::3,::4)   ! <<< Unknown as (1:2:3,1:3:4) is contiguous and has one element.
P => B(i)%D(:,::2,::2)   ! { dg-error "Assignment to contiguous pointer from non-contiguous target" }
P => C(::2,::2,::2)      ! { dg-error "Assignment to contiguous pointer from non-contiguous target" }

! This following is stricter:
! C1541  The actual argument corresponding to a dummy pointer with the
!        CONTIGUOUS attribute shall be simply contiguous (9.5.4).
call foo (B(i)%A(:,::3,::4))  ! { dg-error "must be simply contiguous" }
call foo (C(::2,::2,::2))     ! { dg-error "must be simply contiguous" }

contains
  subroutine foo(Q)
    integer, pointer, intent(in), contiguous :: Q(:,:,:)
  end subroutine foo
end
