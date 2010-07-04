! { dg-do compile }
! test case contributed by tobias.burnus@physik.fu-berlin.de
! PR28039 Warn when ignoring extra characters in the format specification
       implicit none
       real :: r
       r = 1.0
       write(*,'(a),f)') 'Hello', r   ! { dg-warning "Extraneous characters in format at" }
       end
! Below routine was also submitted by tobias.burnus@physik.fu-berlin.de
! It showed up some problems with the initial implementation of this
! feature.
! This routine should compile without complaint or warning.
      SUBROUTINE rw_inp()
      CHARACTER(len=100) :: line
      integer :: i5
      character(100), parameter :: subchapter =
     &        '(79("-"),/,5("-")," ",A,/,79("-"),/)'
      i5 = 1

      READ(*,FMT="(4x,a)") line
 7182 FORMAT (a3)
 7130 FORMAT (i3)

      WRITE (6,'(//'' icorr is not correctly transferred.  icorr='',i5)
     &    ') 42

      write(*,subchapter) 'test'
      END SUBROUTINE rw_inp
