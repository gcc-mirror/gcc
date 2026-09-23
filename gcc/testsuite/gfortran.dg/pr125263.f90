! { dg-do run }
!
! Test the fix for pr125263, in which the selector expressions were not
! correctly set after the first two ASSOCIATE constructs below.
!
! Conributed by Bastiaan Braams  <b.j.braams@cwi.nl>
!
program Main
  implicit none (type, external)
  type :: Foo_Type
     integer, allocatable :: x(:)
  end type Foo_Type
  class (Foo_Type), allocatable :: fv(:), f, g
  integer :: nx = 2, nf = 3, i

  ! Create fv(:) with all component vectors initialized to 0.
  allocate (Foo_Type::fv(0:nf-1))
  do i = 0, nf-1
     allocate (fv(i)%x(0:nx-1))
     fv(i)%x(:) = 0
  end do

  ! Create f with f%x(:) equal to 1 and g with g%x(:) equal to 2.
  allocate (Foo_Type::f, g)
  allocate (f%x(0:nx-1),g%x(0:nx-1))
  f%x(:) = 1
  g%x(:) = 2

  ! Use intrinsic assignment to copy f to fv(0).
  associate (ft => fv(0))
    select type (ft => fv(0))
    type is (Foo_Type)
       ft = f
       ft%x = [2,3,4]
    class default
       error stop 'select type (ft): type error'
    end select
  end associate

  ! Verify the copy on the element x(0) and that f is not overwritten.
  if (any (fv(0)%x /= [2,3,4])) stop 1
  if (any (f%x /= [1,1])) stop 2

  ! All scalar selector-exprs have the same problem, not just array elements.
  f%x(:) = 1
  associate (ft => g)
    select type (ft)
    type is (Foo_Type)
       ft = f
       ft%x = [4,5,6]
    class default
       error stop 'select type (ft): type error'
    end select
  end associate
  ! Verify the copy on g and that f is not overwritten.
  if (any (g%x /= [4,5,6])) stop 3
  if (any (f%x /= [1,1])) stop 4

  ! Assignment to an element of an array associate name was OK.
  fv(0)%x(:) = [0,0,0]
  select type (ft => fv)
  type is (Foo_Type)
    ft = f
    ft(0)%x = [2,3,4]
  class default
    error stop 'select type (ft): type error'
  end select
  if (any (fv(0)%x /= [2,3,4])) stop 5
  if (any (f%x /= [1,1])) stop 6

end program Main
