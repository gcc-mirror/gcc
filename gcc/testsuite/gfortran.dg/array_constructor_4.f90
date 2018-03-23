! { dg-do run }
! PR 21912
! We didn't adapt the exit condition to negative steps in array constructors,
! leaving the resulting arrays uninitialized.
integer :: i(5), n, m, l, k

n = 5
i = (/ (m, m = n, 1, -1) /)
if (any (i /= (/ 5, 4, 3, 2, 1 /))) STOP 1

k = 1

i(5:1:-1) = (/ (m, m = n, k, -1) /)
if (any (i /= (/ 1, 2, 3, 4, 5 /))) STOP 2

l = -1

i = (/ (m, m = n, 1, l) /)
if (any (i /= (/ 5, 4, 3, 2, 1 /))) STOP 3

i(5:1:-1) = (/ (m, m = n, k, l) /)
if (any (i /= (/ 1, 2, 3, 4, 5 /))) STOP 4
end
