! { dg-do compile }
! { dg-options "-fdump-tree-original" }
!
! PR fortran/54608
!
! Contributed by James Van Buskirk
!
module m1
   implicit none
   contains
      subroutine s1(A)
         logical A
         integer iscan, iverify
         character(7), parameter :: tf(2) = ['.FALSE.','.TRUE. ']

         iscan = scan('AA','A',back=A)
         iverify = verify('xx','A',back=A)
         if (iscan /= 2 .or. iverify /= 2) call abort ()
         print *, iverify, iscan
!         write(*,'(a)') 'SCAN test: A = '//trim(tf(iscan)) ! should print true
!         write(*,'(a)') 'VERIFY test: A = '//trim(tf(iverify)) ! should print true
      end subroutine s1
end module m1

program p1
   use m1
   implicit none
   logical B

   call s1(.TRUE.)
end program p1

! { dg-final { scan-tree-dump-times "iscan = _gfortran_string_scan \\(2," 1 "original" } }
! { dg-final { scan-tree-dump-times "iverify = _gfortran_string_verify \\(2," 1 "original" } }
! { dg-final { cleanup-tree-dump "original" } }
