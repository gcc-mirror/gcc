! Test the dependency checking in simple where. This
! did not work and was fixed as part of the patch for
! pr24519.
!
program where_20
   integer :: a(4)
   integer :: b(3)
   integer :: c(3)
   integer :: d(3) = (/1, 2, 3/)
   equivalence (a(1), b(1)), (a(2), c(1))

! This classic case worked before the patch.
   a = (/1, 2, 3, 4/)
   where (b .gt. 1) a(2:4) = a(1:3)
   if (any(a .ne. (/1,2,2,3/))) STOP 1

! This is the original manifestation of the problem
! and is repeated in where_19.f90.
   a = (/1, 2, 3, 4/)
   where (b .gt. 1)
     c = b
   endwhere
   if (any(a .ne. (/1,2,2,3/))) STOP 2

! Mask to.destination dependency.
   a = (/1, 2, 3, 4/)
   where (b .gt. 1)
     c = d
   endwhere
   if (any(a .ne. (/1,2,2,3/))) STOP 3

! Source to.destination dependency.
   a = (/1, 2, 3, 4/)
   where (d .gt. 1)
     c = b
   endwhere
   if (any(a .ne. (/1,2,2,3/))) STOP 4

! Check the simple where.
   a = (/1, 2, 3, 4/)
   where (b .gt. 1) c = b
   if (any(a .ne. (/1,2,2,3/))) STOP 5

! This was OK before the patch.
   a = (/1, 2, 3, 4/)
   where (b .gt. 1)
     where (d .gt. 1)
       c = b
     end where
   endwhere
   if (any(a .ne. (/1,2,2,3/))) STOP 6

end program

