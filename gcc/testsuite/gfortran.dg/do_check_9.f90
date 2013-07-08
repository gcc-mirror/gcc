! { dg-do compile }
!
! PR fortran/50554
!
! Contributed by Vittorio Zecca
!
! INQUIRE cannot redefine DO index
!
      do I=1,10 ! { dg-error "cannot be redefined inside loop beginning at" }
       inquire(iolength=I) n ! { dg-error "cannot be redefined inside loop beginning at" }
       inquire(99,size=I) ! { dg-error "cannot be redefined inside loop beginning at" }
       read(99,'(i4)',size=I,advance="no") n ! { dg-error "cannot be redefined inside loop beginning at" }
      end do
      end
