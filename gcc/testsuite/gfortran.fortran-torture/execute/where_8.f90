program where_8
  implicit none
  type t
    logical valid
    integer :: s
    integer, dimension(8) :: p
  end type
  type (t), dimension (5) :: v
  integer i

  v(:)%valid = (/.true., .true., .false., .true., .true./)
  v(:)%s = (/1, 8, 999, 6, 2/)
  v(1)%p(:) = (/9, 10, 0, 0, 0, 0, 0, 0/)
  v(2)%p(:) = (/1, 2, 3, 4, 5, 6, 7, 8/)
  v(4)%p(:) = (/13, 14, 15, 16, 17, 18, 19, 20/)
  v(5)%p(:) = (/11, 12, 0, 0, 0, 0, 0, 0/)

  forall (i=1:5,v(i)%valid)
    where (v(i)%p(1:v(i)%s).gt.4)
      v(i)%p(1:v(i)%s) = 21
    end where
  end forall

  if (any(v(1)%p(:) .ne. (/21, 10, 0, 0, 0, 0, 0, 0/))) call abort
  if (any(v(2)%p(:) .ne. (/1, 2, 3, 4, 21, 21, 21, 21/))) call abort
  if (any(v(4)%p(:) .ne. (/21, 21, 21, 21, 21, 21, 19, 20/))) call abort
  if (any(v(5)%p(:) .ne. (/21, 21, 0, 0, 0, 0, 0, 0/))) call abort
end program
