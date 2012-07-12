! { dg-do compile }
!
! PR fortran/53732
! this was leading to an internal "mismatching comparison operand types"
! error.
!
! Original testcase by minzastro <minzastro@googlemail.com>
! Fixed by Dominique Dhumieres <dominiq@lps.ens.fr>

program test
implicit none

real(8) arr(4,4,4,4)

arr(:,:,:,:) = 1d0

arr(1,:,:,:) = sum(arr, dim=1, mask=(arr(:,:,:,:) > 0d0))

end program test
