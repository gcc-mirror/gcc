! { dg-do compile }
! { dg-options "-O2 -fno-tree-dominator-opts -fdump-tree-pre -fno-tree-sra" }
module test

   type shell1quartet_type

   integer(kind=kind(1)) :: ab_l_sum
   integer(kind=kind(1)), dimension(:), pointer :: ab_form_3dints_x_indices => NULL()
   integer(kind=kind(1)), dimension(:), pointer :: ab_form_3dints_yz_rms_indices => NULL()

    end type

contains
subroutine make_esss(self,esss)
  type(shell1quartet_type) :: self
  intent(in) :: self
  real(kind=kind(1.0d0)), dimension(:), intent(out) :: esss
  real(kind=kind(1.0d0)), dimension(:), pointer :: Izz
  real(kind=kind(1.0d0)), dimension(:,:), pointer :: Ix,Iy,Iz,Iyz
  integer(kind=kind(1)), dimension(:), pointer  :: e_x,ii_ivec
  integer(kind=kind(1)) :: dim, dim1, nroots, ii,z,y

    dim = self%ab_l_sum+1
    dim1 = self%ab_l_sum+2
    nroots = (dim1) / 2
    call create_(Ix,nroots,dim)
    call create_(Iy,nroots,dim)
    call create_(Iz,nroots,dim)
    call create_(Iyz,nroots,dim*dim1/2)

    e_x => self%ab_form_3dints_x_indices
    ii_ivec => self%ab_form_3dints_yz_rms_indices

    call foo(Ix)
    call foo(Iy)
    call foo(Iz)

    esss = ZERO
    ii = 0
    do z=1,dim
      Izz => Iz(:,z)
      do y=1,dim1-z
        ii = ii + 1
        Iyz(:,ii) = Izz * Iy(:,y)
      end do
    end do
    esss = esss + sum(Ix(:,e_x) * Iyz(:,ii_ivec),1)

end subroutine

end

! There should be two loads from iyz.data, not four.

! { dg-final { scan-tree-dump-times "= iyz.data" 2 "pre" } }
