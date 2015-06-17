subroutine mv(m,nc,irp,ja,val,x,ldx,y,ldy,acc)
  use iso_fortran_env
  implicit none 

  integer, parameter  :: ipk_ = int32
  integer, parameter  :: spk_   = real32
  complex(spk_), parameter   :: czero=(0.0_spk_,0.0_spk_)

  integer(ipk_), intent(in)      :: m,ldx,ldy,nc,irp(*),ja(*)
  complex(spk_), intent(in)      :: x(ldx,*),val(*)
  complex(spk_), intent(inout)   :: y(ldy,*)
  complex(spk_), intent(inout)   :: acc(*)
  integer(ipk_) :: i,j,k, ir, jc
    
  do i=1,m 
    acc(1:nc)  = czero
    do j=irp(i), irp(i+1)-1
      acc(1:nc)  = acc(1:nc) + val(j) * x(ja(j),1:nc)          
    enddo
    y(i,1:nc) = -acc(1:nc)
  end do
  
end subroutine mv
