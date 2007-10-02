! { dg-do compile }
! Tests the fix for PR33566, in which the first variable array ref
! to v1 would cause an incompatible ranks error and the second an ICE.
!
! Contributed by Mikael Morin <mikael.morin@tele2.fr>
!
      program test_vec

      implicit none


      integer :: i
      real    :: x

      type vec3
        real, dimension(3) :: coords
      end type vec3

      type(vec3),parameter :: v1 = vec3((/ 1.0, 2.0, 3.0 /))
      type(vec3)           :: v2

      v2 = vec3((/ 1.0, 2.0, 3.0 /))


      x = v1%coords(1)

      do i=1,3
        x = v1%coords(i)  ! used to fail
        x = v2%coords(i)
      end do

      i = 2

      v2 = vec3 (v1%coords ((/i+1, i, i-1/))) ! also broken

      end program test_vec
