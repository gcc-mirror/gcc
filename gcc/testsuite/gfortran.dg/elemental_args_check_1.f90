! { dg-do compile }
! PR fortran/33343
!
! Check conformance of array actual arguments to
! elemental function.
!
! Contributed by Mikael Morin  <mikael.morin@tele2.fr>
!
      module geometry
      implicit none
      integer, parameter :: prec = 8
      integer, parameter :: length = 10
      contains
      elemental function Mul(a, b)
      real(kind=prec) :: a
      real(kind=prec) :: b, Mul
      intent(in)      :: a, b
      Mul = a * b
      end function Mul

      pure subroutine calcdAcc2(vectors, angles)
      real(kind=prec),      dimension(:)          :: vectors
      real(kind=prec), dimension(size(vectors),2) :: angles
      intent(in) :: vectors, angles
      real(kind=prec), dimension(size(vectors)) :: ax
      real(kind=prec), dimension(size(vectors),2) :: tmpAcc
      tmpAcc(1,:) = Mul(angles(1,1:2),ax(1)) ! Ok
      tmpAcc(:,1) = Mul(angles(:,1),ax)      ! OK
      tmpAcc(:,:) = Mul(angles(:,:),ax) ! { dg-error "Incompatible ranks in elemental procedure" }
      end subroutine calcdAcc2
      end module geometry
