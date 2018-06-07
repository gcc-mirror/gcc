! { dg-do run }
! { dg-options "-fcoarray=single" }
program rantest

   implicit none

   logical, parameter :: debug = .false.
   character(len=20) name
   integer fd, i, n
   integer, allocatable :: n1(:), n2(:), n3(:)
   real x(4), y(4), z(4)

   if (debug) then
      write(name,'(A,I0)') 'dat', this_image()
      open(newunit=fd, file=name)
   end if

   call random_seed(size=n)
   allocate(n1(n), n2(n), n3(n))
   !
   ! Setup repeatable sequences (if co-arrays the seeds should be distinct
   ! are different).  Get the seeds.
   !
   call random_init(.true., .true.)
   call random_seed(get=n1)
   call random_number(x)               ! This changes internal state.
   if (debug) then
      write(fd,'(A,4F12.6)') 'x = ', x
   end if

   call random_seed(get=n2)            ! Grab current state. 
   !
   ! Use the gotten seed to reseed PRNG and grab sequence.
   ! It should be the same sequence.
   !
   call random_seed(put=n1)
   call random_number(y)
   if (debug) then
      write(fd,'(A,4F12.6)') 'y = ', y
   end if
   !
   ! Setup repeatable sequences (if co-arrays the seeds should be distinct
   ! are different).  Get the seeds.  It should be the same sequence.
   !
   call random_init(.true., .true.)
   call random_seed(get=n3)
   call random_number(z)
   if (debug) then
      write(fd,'(A,4F12.6)') 'z = ', z
   end if

   x = int(1e6*x) ! Convert to integer with at most 6 digits.
   y = int(1e6*y) ! Convert to integer with at most 6 digits.
   z = int(1e6*z) ! Convert to integer with at most 6 digits.

   if (any(x /= y)) call abort
   if (any(x /= z)) call abort

   if (debug) then
      write(fd,*)
      do i = 1, n
         if (n1(i) - n2(i) /= 0) then
            write(fd,*) 'n1 /= n2', i, n1(i), n2(i)
         end if
      end do
      write(fd,*)
      do i = 1, n
         if (n1(i) - n3(i) /= 0) then
            write(fd,*) 'n1 /= n3', i, n1(i), n3(i)
         end if
      end do
   end if

end program rantest
