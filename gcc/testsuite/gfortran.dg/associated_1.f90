! { dg-do run }
! PR 25292: Check that the intrinsic associated works with functions returning
! pointers as arguments
program test
   real, pointer :: a, b

   nullify(a,b)
   if(associated(a,b).or.associated(a,a)) STOP 1
   allocate(a)
   if(associated(b,a)) STOP 2
   if (.not.associated(x(a))) STOP 3
   if (.not.associated(a, x(a))) STOP 4

   nullify(b)
   if (associated(x(b))) STOP 5
   allocate(b)
   if (associated(x(b), x(a))) STOP 6

contains

  function x(a) RESULT(b)
    real, pointer :: a,b
    b => a
  end function x

end program test
