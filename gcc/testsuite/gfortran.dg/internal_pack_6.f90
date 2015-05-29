! { dg-do run }
! { dg-options "-fdump-tree-original" }
!
! Test the fix for PR41113 and PR41117, in which unnecessary calls
! to internal_pack and internal_unpack were being generated.
!
! Contributed by Joost VandeVondele <jv244@cam.ac.uk>
!
MODULE M1
 TYPE T1
   REAL :: data(10) = [(i, i = 1, 10)]
 END TYPE T1
CONTAINS
 SUBROUTINE S1(data, i, chksum)
   REAL, DIMENSION(*) :: data
   integer :: i, j
   real :: subsum, chksum
   subsum = 0
   do j = 1, i
     subsum = subsum + data(j)
   end do
   if (abs(subsum - chksum) > 1e-6) call abort
 END SUBROUTINE S1
END MODULE

SUBROUTINE S2
 use m1
 TYPE(T1) :: d

 real :: data1(10) = [(i, i = 1, 10)]
 REAL :: data(-4:5,-4:5) = reshape ([(real(i), i = 1, 100)], [10,10])

! PR41113
 CALL S1(d%data, 10, sum (d%data))
 CALL S1(data1, 10, sum (data1))

! PR41117
 DO i=-4,5
    CALL S1(data(:,i), 10, sum (data(:,i)))
 ENDDO

! With the fix for PR41113/7 this is the only time that _internal_pack
! was called.  The final part of the fix for PR43072 put paid to it too.
 DO i=-4,5
    CALL S1(data(-2:,i), 8, sum (data(-2:,i)))
 ENDDO
 DO i=-4,4
    CALL S1(data(:,i:i+1), 20, sum (reshape (data(:,i:i+1), [20])))
 ENDDO
 DO i=-4,5
    CALL S1(data(2,i), 1, data(2,i))
 ENDDO
END SUBROUTINE S2

 call s2
end
! { dg-final { scan-tree-dump-times "_gfortran_internal_pack" 0 "original" } }
