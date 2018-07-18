! Program to test FORALL construct
program forall_1

   call actual_variable ()
   call negative_stride ()
   call forall_index ()

contains
   subroutine actual_variable ()
      integer:: x = -1
      integer a(3,4)
      j = 100
      
      ! Actual variable 'x' and 'j' used as FORALL index
      forall (x = 1:3, j = 1:4)
         a (x,j) = j
      end forall
      if (any (a.ne.reshape ((/1,1,1,2,2,2,3,3,3,4,4,4/), (/3,4/)))) STOP 1
      if ((x.ne.-1).or.(j.ne.100)) STOP 2

      call actual_variable_2 (x, j, a)
   end subroutine

   subroutine actual_variable_2(x, j, a)
      integer x,j,x1,j1
      integer a(3,4), b(3,4)

      ! Actual variable 'x' and 'j' used as FORALL index.
      forall (x=3:1:-1, j=4:1:-1)
         a(x,j) = j
         b(x,j) = j
      end forall

      if (any (a.ne.reshape ((/1,1,1,2,2,2,3,3,3,4,4,4/), (/3,4/)))) STOP 3
      if (any (b.ne.reshape ((/1,1,1,2,2,2,3,3,3,4,4,4/), (/3,4/)))) STOP 4
      if ((x.ne.-1).or.(j.ne.100)) STOP 5
   end subroutine

   subroutine negative_stride ()     
      integer a(3,4)
      integer x, j

      ! FORALL with negative stride
      forall (x = 3:1:-1, j = 4:1:-1)
         a(x,j) = j + x
      end forall
      if (any (a.ne.reshape ((/2,3,4,3,4,5,4,5,6,5,6,7/), (/3,4/)))) STOP 6
   end subroutine

   subroutine forall_index
      integer a(32,32)

      ! FORALL with arbitrary number indexes
      forall (i1=1:2,i2=1:2,i3=1:2,i4=1:2,i5=1:2,i6=1:2,i7=1:2,i8=1:2,i9=1:2,&
              i10=1:2)
         a(i1+2*i3+4*i5+8*i7+16*i9-30,i2+2*i4+4*i6+8*i8+16*i10-30) = 1
      end forall
      if ((a(5,5).ne.1).or. (a(32,32).ne.1)) STOP 7
   end subroutine

end
