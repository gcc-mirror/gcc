! { dg-do compile }
subroutine foo ()
   real(4), allocatable, save :: tmp (:, :)
   real(4), pointer, save :: arr (:, :, :)
   integer :: l, m, n
   tmp = (cshift(cshift(arr (:,:,l),m,2),n,1))
end subroutine foo
