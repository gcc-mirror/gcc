! Check more array variants of the data statement
program data_2
  implicit none
  type t
    integer i
  end type t
  integer, dimension(3) :: a
  type (t), dimension(3) :: b
  integer, dimension(2,2) :: c
  data a(:), b%i /1, 2, 3, 4, 5, 6/
  data c(1, :), c(2, :) /7, 8, 9, 10/

  if (any (a .ne. (/1, 2, 3/))) call abort ()
  if (any (b%i .ne. (/4, 5, 6/))) call abort ()
  if ((any (c(1, :) .ne. (/7, 8/))) &
      .or. (any (c(2,:) .ne. (/9, 10/)))) call abort ()
end program
