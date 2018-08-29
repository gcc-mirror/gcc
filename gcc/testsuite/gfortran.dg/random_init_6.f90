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

   call random_init(.false., .true.)
   call random_seed(get=n1)
   call random_number(x)

   call random_init(.false., .true.)
   call random_seed(get=n2)
   call random_number(y)

   call random_init(.false., .true.)
   call random_seed(get=n3)
   call random_number(z)

   if (debug) then
      write(fd,'(A,4F12.6)') 'x = ', x
      write(fd,'(A,4F12.6)') 'y = ', y
      write(fd,'(A,4F12.6)') 'z = ', z
      write(fd,*)
      do i = 1, 5
         write(fd,'(I2,4I13)') i, n1(i), n2(i), n3(i)
      end do
   end if

end program rantest
