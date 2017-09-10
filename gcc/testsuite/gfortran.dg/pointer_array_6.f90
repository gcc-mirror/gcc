! { dg-do run }
!
! Test the fix for PR57019 comment 4 as part of the overall fix for PR34640.
!
! Contributed by  <thambsup@gmail.com>
!
  type cParticle
    real(4) :: v(3)
  endtype cParticle

  type pCItem
    type(cParticle) :: Ele
  end type pCItem

  type(pCItem), target, dimension(1:1,1:1) :: pCellArray
  type(cParticle), pointer, dimension(:,:) :: pArray
  real(4), pointer, dimension(:) :: v_pointer
  real(4), dimension(3) :: v_real = 99.

  pArray => pCellArray%Ele
  v_pointer => pArray(1,1)%v;
  v_pointer = v_real !OK %%%%%%%%%%%%
  if (any (int (pArray(1,1)%v) .ne. 99)) call abort

  v_real = 88
  pArray(1,1)%v = v_real !SEGFAULT %%%%%%%%%%%%%%%%%%%%%%%%
  if (any (int (v_pointer) .ne. 88)) call abort
end
