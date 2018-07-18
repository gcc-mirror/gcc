program foo
   integer, dimension (2, 3) :: a
   integer, dimension (2, 2, 3) :: b
   character (len=80) line1, line2, line3

   a = reshape ((/1, 2, 3, 4, 5, 6/), (/2, 3/))
   b = spread (a, 1, 2)
   if (any (b .ne. reshape ((/1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6/), &
                            (/2, 2, 3/)))) &
      STOP 1
   write(line1, 9000) b
   write(line2, 9000) spread (a, 1, 2)
   if (line1 /= line2) STOP 2
   write(line3, 9000) spread (a, 1, 2) + 0
   if (line1 /= line3) STOP 3
9000 format(12I3)
end program
