! { dg-do run }
! PR 21875 : Test formatted input/output to/from character arrays.
! Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
      program arrayio_1
      implicit none
      integer         :: i(6),j,k
      character(12)  :: r(12,2) = '0123456789AB'
 
! Write to and read from a whole character array

      i = (/(j,j=1,6)/)
      write(r,'(3(2x,i4/)/3(3x,i6/))') i
      i = 0
      read(r,'(3(2x,i4/)/3(3x,i6/))') i
      if (any(i.ne.(/(j,j=1,6)/))) call abort()
      do j=1,12
         do k=1,2
            if ((j.gt.8.and.k.eq.1).or.(k.eq.2)) then
              if (r(j,k).ne.'0123456789AB') call abort()
            end if
         end do
      end do

 ! Write to a portion of a character array      
      r = '0123456789AB'
      write(r(3:9,1),'(6(i12/))') i
      if (r(2,1).ne.'0123456789AB') call abort()
      do j=3,8
        if (iachar(trim(adjustl(r(j,1))))-46.ne.j) call abort()
      end do
      if (r(9,1).ne.'            ') call abort()
      end program arrayio_1
