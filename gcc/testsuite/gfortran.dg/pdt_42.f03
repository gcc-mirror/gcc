! { dg-do run )
!
! Test the fix for PR87669 in which SELECT TYPE was not identifying the difference
! between derived types with different type kind parameters, when the selector
! is unlimited polymorphic.
!
! Contributed by Etienne Descamps  <etdescdev@gmail.com>
!
Program Devtest
  Type dvtype(k)
    Integer, Kind :: k
    Real(k) :: a, b, c
  End Type dvtype
  type(dvtype(8)) :: dv
  type(dvtype(4)) :: fv
  integer :: ctr = 0

  dv%a = 1; dv%b = 2; dv%c = 3
  call dvtype_print(dv)
  if (ctr /= 2) stop 1

  fv%a = 1; fv%b = 2; fv%c = 3
  call dvtype_print(fv)
  if (ctr /= 0) stop 2

Contains
  Subroutine dvtype_print(p)
    class(*), intent(in) :: p
    Select Type(p)
    class is (dvtype(4))
      ctr = ctr - 1
    End Select
    Select Type(p)
    class is (dvtype(8))
      ctr = ctr + 1
    End Select
    Select Type(p)
    type is (dvtype(4))
      ctr = ctr - 1
    End Select
    Select Type(p)
    type is (dvtype(8))
      ctr = ctr + 1
    End Select
  End Subroutine dvtype_print
End
