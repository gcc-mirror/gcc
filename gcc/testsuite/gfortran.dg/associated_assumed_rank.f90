! { dg-do run }

! PR fortran/101334

implicit none (type, external)
real, target :: AT(10,10), BT
real, contiguous, pointer :: A(:,:)
real, pointer :: B
real, pointer :: AP(:,:), BP
real, pointer :: CP(:), DP(:,:), D, EP(:)

call test_char()

A => AT
B => BT

AP => A
BP => B
call foo(AP,B, A, 1) ! OK - associated
call foo(BP,B, A, 2) !  OK - associated

! Those are all not associated:

AP => null()
BP => null()
call foo(AP, B, A, 3) ! LHS not associated
call foo(BP, B, A, 4) ! LHS not associated

DP => null()
D => null()
call foo(AP, B, DP, 5) ! LHS+RHS not associated
call foo(BP, D, A, 6)  ! LHS+RHS not associated

AP => A
BP => B
call foo(AP, B, DP, 7) ! RHS not associated
call foo(BP, D, A, 8)  ! RHS not associated

CP(1:size(A)) => A
call foo(CP, B, A, 9)  ! Shape (rank) differs

AP => A(2:,:)
call foo(AP, B, A, 10)  ! Shape differs

AP => A(:,2:)
call foo(AP, B, A, 11)  ! Shape differs

AP(10:,10:) => A
call foo(AP, B, A, 12)  ! OK - bounds different, shape same

CP => AT(1:-1, 5)
EP => AT(1:-1, 5)        ! Case(i) + case(iv)
call foo2(CP, EP)  ! CP associated - but CP not associated with EP
contains
subroutine foo2(p, lpd)
  implicit none (type, external)
  real, pointer :: p(..)    ! "pointer"
  real, pointer :: lpd(:) ! array "target"
  if (.not.associated(p)) stop 18 ! OK - associated 
  if (associated(p, lpd)) stop 19 ! .. but for zero-sized array
end

subroutine foo(p, lp, lpd, cnt)
  implicit none (type, external)
  real, pointer :: p(..)    ! "pointer"
  real, pointer :: lp       ! scalar "target"
  real, pointer :: lpd(:,:) ! array "target"
  integer, value :: cnt

  if (cnt == 1) then
    if (.not. associated(p, lpd)) stop 1  ! OK
  elseif (cnt == 2) then
    if (.not. associated(p, lp)) stop 2   ! OK
  elseif (cnt == 3) then
    if (associated(p, lpd)) stop 3 ! LHS NULL ptr
    if (associated(p)) stop 4      ! LHS NULL ptr
  elseif (cnt == 4) then
    if (associated(p, lp)) stop 5  ! LHS NULL ptr
    if (associated(p)) stop 6      ! LHS NULL ptr
  elseif (cnt == 5) then
    if (associated(p, lpd)) stop 7 ! LHS+RHS NULL ptr
    if (associated(p)) stop 8      ! LHS+RHS NULL ptr
  elseif (cnt == 6) then
    if (associated(p, lp)) stop 9  ! LHS+RHS NULL ptr
    if (associated(p)) stop 10      ! LHS+RHS NULL ptr
  elseif (cnt == 7) then
    if (associated(p, lpd)) stop 11 ! RHS NULL ptr
  elseif (cnt == 8) then
    if (associated(p, lp)) stop 12  ! RHS NULL ptr
  elseif (cnt == 9) then
    if (associated(p, lpd)) stop 13 ! rank differs
    if (associated(p, lp)) stop 14  ! rank differs
  elseif (cnt == 10) then
    if (associated(p, lpd)) stop 15 ! shape differs
  elseif (cnt == 11) then
    if (associated(p, lpd)) stop 16 ! shape differs
  elseif (cnt == 12) then
    if (.not.associated(p, lpd)) stop 17 ! OK - shape same, lbound different
  else
    stop 99
  endif
end 
subroutine test_char()
  character(len=0), target :: str0
  character(len=2), target :: str2
  character(len=:), pointer :: ptr
  ptr => str0
  call test_char2(ptr, str0)
  ptr => str2
  call test_char2(ptr, str2)
end
subroutine test_char2(x,y)
  character(len=:), pointer :: x
  character(len=*), target :: y
  if (len(y) == 0) then
    if (len(x) /= 0) stop 20
    if (.not. associated(x)) stop 21
    if (associated(x, y)) stop 22
  else
    if (len(y) /= 2) stop 23
    if (len(x) /= 2) stop 24
    if (.not. associated(x)) stop 25
    if (.not. associated(x, y)) stop 26
  end if
end
end
