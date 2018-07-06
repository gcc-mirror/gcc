! { dg-do run }
program p
   character(len=20) s1, s2
   integer, allocatable :: n(:)
   n = [2,1]
   s1 = '1 5 2 6 3 0 4 0'
   write(s2,'(8(I0,1x))') reshape ([1,2,3,4,5,6], [2,4], [0,0], [2,1])
   if (trim(s1) /= trim(s2)) STOP 1
   write(s2,'(8(I0,1x))') reshape ([1,2,3,4,5,6], [2,4], [0,0], n)
   if (trim(s1) /= trim(s2)) STOP 2
   write(s2,'(8(I0,1x))') reshape ([1,2,3,4,5,6], [2,4], [0,0], [n])
   if (trim(s1) /= trim(s2)) STOP 3
end
