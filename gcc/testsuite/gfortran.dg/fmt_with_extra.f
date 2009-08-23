! { dg-do compile }
! test case contributed by tobias.burnus@physik.fu-berlin.de
! PR28039 Warn when ignoring extra characters in the format specification
       implicit none
       real :: r
       r = 1.0
       write(*,'(a),f)') 'Hello', r   !{ dg-warning "Extraneous characters in format at" }
       end
