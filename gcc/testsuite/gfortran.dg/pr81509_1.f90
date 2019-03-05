! { dg-do run }
! https://gcc.gnu.org/bugzilla/show_bug.cgi?id=81509
program foo
logical :: a = .false.
integer :: i = 42
integer(8) :: k = 42
if (kind(ieor(z'ade',i)) /= 4) call abort
if (kind(ior(i,z'1111')) /= 4) call abort
if (kind(ior(1_8,k)) /= 8) call abort
if (kind(iand(k,b'1111')) /= 8) call abort
end program foo

