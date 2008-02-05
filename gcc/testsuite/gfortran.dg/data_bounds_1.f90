! { dg-do compile }
! Checks the fix for PR32315, in which the bounds checks below were not being done.
!
! Contributed by Tobias Burnus <burnus@gcc.gnu.org>
!
program chkdata
    character(len=20), dimension(4) :: string
    character(len=20), dimension(0:1,3:4) :: string2

    data (string(i) ,i = 4, 5) /'D', 'E'/ ! { dg-error "above array upper bound" }
    data (string(i) ,i = 0, 1) /'A', 'B'/ ! { dg-error "below array lower bound" }
    data (string(i) ,i = 1, 4) /'A', 'B', 'C', 'D'/

    data ((string2(i, j) ,i = 1, 2), j = 3, 4) /'A', 'B', 'C', 'D'/ ! { dg-error "above array upper bound" }
    data ((string2(i, j) ,i = 0, 1), j = 2, 3) /'A', 'B', 'C', 'D'/ ! { dg-error "below array lower bound" }
    data ((string2(i, j) ,i = 0, 1), j = 3, 4) /'A', 'B', 'C', 'D'/
end program chkdata
