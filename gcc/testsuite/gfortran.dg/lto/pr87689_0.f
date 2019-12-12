! { dg-lto-do run }
! PR 87689 - this used to fail for POWER, plus it used to
! give warnings about mismatches with LTO.
! Original test case by Judicaël Grasset.
      program main
        implicit none
        character :: c
        character(len=20) :: res, doesntwork_p8
        external doesntwork_p8
        c = 'o'
        res = doesntwork_p8(c,1,2,3,4,5,6)
        if (res /= 'foo') stop 3
      end program main
