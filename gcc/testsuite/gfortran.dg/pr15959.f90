! { dg-do run }
! Test initializer of character array. PR15959
character (*), parameter :: a (1:2) = (/'ab ', 'abc'/)
if (a(2) .ne. 'abc') call abort()
end
