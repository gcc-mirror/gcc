program foo
   integer, dimension (2, 3) :: a
   integer, dimension (2, 2, 3) :: b

   a = reshape ((/1, 2, 3, 4, 5, 6/), (/2, 3/))
   b = spread (a, 1, 2)
   if (any (b .ne. reshape ((/1, 1, 2, 2, 3, 3, 4, 4, 5, 5, 6, 6/), &
                            (/2, 2, 3/)))) &
      call abort
end program
