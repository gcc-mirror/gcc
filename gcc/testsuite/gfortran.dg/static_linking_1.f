! { dg-require-effective-target static_libgfortran }
! { dg-do run }
! { dg-additional-sources static_linking_1.c }
! { dg-options "-static" }
!
! This testcase checks that statically linking libgfortran with C main()
! really calls the constructor function
! PR libfortran/22298
      subroutine f
        print *, "subroutine output"
      end
