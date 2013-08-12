! { dg-do run }
! { dg-options "-fcoarray=single -Wzerotrip" }
!
! PR fortran/18918
!
! Contributed by John Reid.
!
program ex2
      implicit none
      real, allocatable :: z(:)[:]
      integer :: image
      character(len=128) :: str

      allocate(z(3)[*])
      write(*,*) 'z allocated on image',this_image()
      sync all
      if (this_image()==1) then
          z = 1.2
          do image = 2, num_images() ! { dg-warning "will be executed zero times" }
            write(*,*) 'Assigning z(:) on image',image
            z(:)[image] = z
         end do
      end if
      sync all

      str = repeat('X', len(str))
      write(str,*) 'z=',z(:),' on image',this_image()
      if (str /= " z=   1.20000005       1.20000005       1.20000005      on image           1") &
        call abort

      str = repeat('X', len(str))
      write(str,*) 'z=',z,' on image',this_image()
      if (str /= " z=   1.20000005       1.20000005       1.20000005      on image           1") &
        call abort

      str = repeat('X', len(str))
      write(str,*) 'z=',z(1:3)[this_image()],' on image',this_image()
      if (str /= " z=   1.20000005       1.20000005       1.20000005      on image           1") &
        call abort

      call ex2a()
      call ex5()
end

subroutine ex2a()
      implicit none
      real, allocatable :: z(:,:)[:,:]
      integer :: image
      character(len=128) :: str

      allocate(z(2,2)[1,*])
      write(*,*) 'z allocated on image',this_image()
      sync all
      if (this_image()==1) then
          z = 1.2
          do image = 2, num_images() ! { dg-warning "will be executed zero times" }
            write(*,*) 'Assigning z(:) on image',image
            z(:,:)[1,image] = z
         end do
      end if
      sync all

      str = repeat('X', len(str))
      write(str,*) 'z=',z(:,:),' on image',this_image()
      if (str /= " z=   1.20000005       1.20000005       1.20000005       1.20000005      on image           1") &
        call abort

      str = repeat('X', len(str))
      write(str,*) 'z=',z,' on image',this_image()
      if (str /= " z=   1.20000005       1.20000005       1.20000005       1.20000005      on image           1") &
        call abort
end subroutine ex2a

subroutine ex5
   implicit none
   integer :: me
   real, save :: w(4)[*]
   character(len=128) :: str

   me = this_image()
   w = me

   str = repeat('X', len(str))
   write(str,*) 'In main on image',this_image(), 'w= ',w 
   if (str /= " In main on image           1 w=    1.00000000       1.00000000       1.00000000       1.00000000") &
        call abort

   str = repeat('X', len(str))
   write(str,*) 'In main on image',this_image(), 'w= ',w(1:4) 
   if (str /= " In main on image           1 w=    1.00000000       1.00000000       1.00000000       1.00000000") &
        call abort

   str = repeat('X', len(str))
   write(str,*) 'In main on image',this_image(), 'w= ',w(:)[1]
   if (str /= " In main on image           1 w=    1.00000000       1.00000000       1.00000000       1.00000000") &
        call abort

   sync all
   call ex5_sub(me,w)
end subroutine ex5
      
subroutine ex5_sub(n,w)
   implicit none
   integer :: n
   real :: w(n)
   character(len=75) :: str

   str = repeat('X', len(str))
   write(str,*) 'In sub on image',this_image(), 'w= ',w 
   if (str /= " In sub on image           1 w=    1.00000000") &
        call abort
end subroutine ex5_sub
