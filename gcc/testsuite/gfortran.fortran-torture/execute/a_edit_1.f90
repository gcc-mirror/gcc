! pr 15113
! Ax edit descriptor x larger than destination
! A edit descriptor with no field width segfaults
       character*16 C
       character*4 D
       data C / 'ABCDEFGHIJKLMNOP'/
       read(C,'(A7)')D
       if (D.NE.'DEFG') then
!         print*,D
          call abort
       endif
       read(C,'(A)')D
       if (D.NE.'ABCD') then
!         print*,D
          call abort
       endif
       end
