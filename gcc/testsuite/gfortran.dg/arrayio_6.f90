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

      if (s(36).ne.'0123456789AB') STOP 1
      if (s(37).ne.'     1      ') STOP 2
      if (s(38).ne.'0123456789AB') STOP 3
      if (s(39).ne.'     2      ') STOP 4
      if (s(40).ne.'0123456789AB') STOP 5
      if (s(41).ne.'     3      ') STOP 6
      if (s(42).ne.'0123456789AB') STOP 7
      if (s(43).ne.'            ') STOP 8
      if (s(44).ne.'0123456789AB') STOP 9
      if (s(45).ne.'            ') STOP 10
      if (s(46).ne.'0123456789AB') STOP 11
 
      k = i
      i = 0
      read(r(1:4:2,2:4:1,3:4:2),'(3(2x,i4/)/3(3x,i6/))') i
      if (any(i.ne.k)) STOP 12
      
      end program arrayio_6
