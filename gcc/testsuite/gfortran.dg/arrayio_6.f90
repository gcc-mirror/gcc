! { dg-do run }
! PR24224 Test formatted input/output to/from character arrays with strides
! other than 1.  Contributed by Jerry DeLisle <jvdelisle@verizon.net>.
      program arrayio_6
      implicit none
      integer         :: i(3),j,k(3)
      character(12)  :: r(4,4,4) = '0123456789AB'
      character(12)  :: s(64)
      equivalence(r,s)
 
      i = (/(j,j=1,3)/)
      write(r(1:4:2,2:4:1,3:4:2),'(3(2x,i4/)/3(3x,i6/))') i

      if (s(36).ne.'0123456789AB') call abort()
      if (s(37).ne.'     1      ') call abort()
      if (s(38).ne.'0123456789AB') call abort()
      if (s(39).ne.'     2      ') call abort()
      if (s(40).ne.'0123456789AB') call abort()
      if (s(41).ne.'     3      ') call abort()
      if (s(42).ne.'0123456789AB') call abort()
      if (s(43).ne.'            ') call abort()
      if (s(44).ne.'0123456789AB') call abort()
      if (s(45).ne.'            ') call abort()
      if (s(46).ne.'0123456789AB') call abort()
 
      k = i
      i = 0
      read(r(1:4:2,2:4:1,3:4:2),'(3(2x,i4/)/3(3x,i6/))') i
      if (any(i.ne.k)) call abort()
      
      end program arrayio_6
