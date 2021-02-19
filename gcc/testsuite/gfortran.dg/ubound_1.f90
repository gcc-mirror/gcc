! { dg-do run }
!
! PR fortran/99027
!
  program test
    integer, dimension (1:3,1:6) :: array
    integer, dimension (2:5,3:7,4:9,-4:2) :: array2

    if (any ([4] /= ubound (array (1, 1:4)))) stop 1
    if (4 /= ubound (array (1, 1:4), dim=1)) stop 2

    if (any (ubound (array2 (3,3,4,:))        /= [4+1+2])) stop 3
    if (     ubound (array2 (3,3,4,:), dim=1) /=  4+1+2 ) stop 4

    if (any (ubound (array2 (3,:,4,:))        /= [7-3+1, 4+1+2])) stop 5
    if (     ubound (array2 (3,:,4,:), dim=1) /=  7-3+1       ) stop 6
    if (     ubound (array2 (3,:,4,:), dim=2) /=         4+1+2) stop 7
    if (any (ubound (array2 (3,:,4:4,:))        /= [7-3+1, 1, 4+1+2])) stop 8
    if (     ubound (array2 (3,:,4:4,:), dim=1) /=  7-3+1          ) stop 9
    if (     ubound (array2 (3,:,4:4,:), dim=2) /=         1       ) stop 10
    if (     ubound (array2 (3,:,4:4,:), dim=3) /=            4+1+2) stop 11
  end program test
