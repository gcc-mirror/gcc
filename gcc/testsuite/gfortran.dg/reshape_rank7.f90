! { dg-do run }
! PR 21075:  Reshape with rank 7 used to segfault.
program main
  integer :: a(256), b(2,2,2,2,2,2,2)
  do i=1,256
     a(i) = i
  end do
  b = reshape(a(1:256:2), shape(b))
  do i1=1,2
     do i2=1,2
        do i3=1,2
           do i4=1,2
              do i5=1,2
                 do i6=1,2
                    do i7=1,2
                       if (b(i1,i2,i3,i4,i5,i6,i7) /= &
                            2*((i1-1)+(i2-1)*2+(i3-1)*4+(i4-1)*8+&
                            (i5-1)*16+(i6-1)*32+(i7-1)*64)+1) &
                            STOP 1
              end do
            end do
          end do
        end do
      end do
    end do
  end do
end program main
