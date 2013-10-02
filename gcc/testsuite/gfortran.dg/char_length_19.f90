! { dg-do compile }
!
! PR fortran/58579
!
! Contributed by Joost VandeVondele
!
! Was ICEing before due to the patch for PR 58593
!
  subroutine test
    CHARACTER(len=20)                        :: tmpStr
    CHARACTER(len=20, kind=4)                :: tmpStr4
    INTEGER :: output_unit=6
       WRITE (UNIT=output_unit,FMT="(T2,A,T61,A20)")&
         "DFT| Self-interaction correction (SIC)",ADJUSTR(TRIM(tmpstr))
       WRITE (UNIT=output_unit,FMT="(T2,A,T61,A20)")&
         4_"DFT| Self-interaction correction (SIC)",ADJUSTR(TRIM(tmpstr4))
   END

!
! PR fortran/58593
! Contributed by Albert Bartok
!
! The PR was overallocating memory. I placed it here to check for a
! variant of the test case above, which takes a slightly differnt code
! patch. Thus, its purpose is just to ensure that it won't ICE.
!
program test_char

   implicit none
   integer :: i

   read*, i
   print*, trim(test(i))

   contains

      function test(i)
         integer, intent(in) :: i
         character(len=i) :: test

         test(1:1) = "A"
      endfunction test

endprogram test_char
