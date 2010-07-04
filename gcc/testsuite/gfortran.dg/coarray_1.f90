! { dg-do compile }
! { dg-options "-std=f2003" }
!
! Coarray support
! PR fortran/18918
!
implicit none
integer :: n
critical  ! { dg-error "Fortran 2008:" }
  sync all()  ! { dg-error "Fortran 2008:" }
end critical ! { dg-error "Expecting END PROGRAM" }
sync memory  ! { dg-error "Fortran 2008:" }
sync images(*)  ! { dg-error "Fortran 2008:" }

! num_images is implicitly defined:
n = num_images()  ! { dg-error "has no IMPLICIT type" }
error stop 'stop'  ! { dg-error "Fortran 2008:" }
end
