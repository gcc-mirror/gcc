! { dg-do compile }
! PR fortran/102715 - ICE in gfc_simplify_transpose

program p
  type t
  end type
  type(t), parameter :: a(4)   = t()
  type(t), parameter :: b(2,2) = reshape(a, [2]) ! { dg-error "Rank mismatch" }
  type(t), parameter :: c(2,2) = transpose(b)    ! { dg-error "must be of rank 2" }
  type(t), parameter :: s2(*)  = b(2,:)          ! { dg-error "Syntax error" }
  type(t), parameter :: x(*,*) = reshape(a, [2]) ! { dg-error "Rank mismatch" }
  type(t), parameter :: s3(*)  = x(2,:)          ! { dg-error "Syntax error" }
end
