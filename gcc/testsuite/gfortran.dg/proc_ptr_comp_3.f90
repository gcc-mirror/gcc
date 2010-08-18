! { dg-do compile }
!
! PR39630: Fortran 2003: Procedure pointer components.
!
! Probing some error messages.
!
! Contributed by Janus Weil <janus@gcc.gnu.org>

implicit none

interface
 subroutine sub
 end subroutine
end interface

external :: aaargh

type :: t
  procedure(), pointer, nopass :: ptr1
  procedure(real), pointer, nopass :: ptr2
  procedure(sub), pointer, nopass :: ptr3
  procedure(), pointer, nopass ptr4              ! { dg-error "Expected '::'" }
  procedure(), pointer, nopass, pointer :: ptr5  ! { dg-error "Duplicate" }
  procedure, pointer, nopass :: ptr6             ! { dg-error "Syntax error" }
  procedure(), nopass :: ptr8                    ! { dg-error "POINTER attribute is required" }
  procedure(pp), pointer, nopass :: ptr9         ! { dg-error "declared in a later PROCEDURE statement" }
  procedure(aaargh), pointer, nopass :: ptr10    ! { dg-error "must be explicit" }
  real :: y
end type t

type,bind(c) :: bct                   ! { dg-error "BIND.C. derived type" }
  procedure(), pointer,nopass :: ptr  ! { dg-error "cannot be a member of|may not be C interoperable" }
end type bct

procedure(sub), pointer :: pp

type(t) :: x

x%ptr2 => x       ! { dg-error "Invalid procedure pointer assignment" }

x => x%ptr2       ! { dg-error "Pointer assignment to non-POINTER" }

print *, x%ptr1() ! { dg-error "attribute conflicts with" }
call x%ptr2()     ! { dg-error "attribute conflicts with" }
print *,x%ptr3()  ! { dg-error "attribute conflicts with" }

call x%y          ! { dg-error "Expected type-bound procedure or procedure pointer component" }

end

